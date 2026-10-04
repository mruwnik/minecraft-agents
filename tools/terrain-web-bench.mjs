#!/usr/bin/env node
// Repeatable, read-only terrain-map benchmark over Chrome DevTools Protocol.
// No browser/server is launched and no dashboard API is mutated.

import fs from 'node:fs/promises';
import path from 'node:path';

const defaults = {
  cdp: 'http://127.0.0.1:9223/json',
  page: '/map?world=claude',
  canvas: 'canvas.map-overlay',
  phaseMs: 8000,
  panX: 300,
  panY: 150,
  panSteps: 5,
  stepMs: 60,
  zoomDelta: 480,
  startZoomIn: 0,
  overviewSteps: 18,
  wheelStepMs: 50,
  screenshots: null,
  out: null,
};

function options(args) {
  const out = {...defaults};
  const names = new Map([
    ['--cdp', 'cdp'], ['--page', 'page'], ['--canvas', 'canvas'],
    ['--phase-ms', 'phaseMs'],
    ['--pan-x', 'panX'], ['--pan-y', 'panY'], ['--pan-steps', 'panSteps'],
    ['--step-ms', 'stepMs'], ['--zoom-delta', 'zoomDelta'],
    ['--start-zoom-in', 'startZoomIn'],
    ['--overview-steps', 'overviewSteps'], ['--wheel-step-ms', 'wheelStepMs'],
    ['--screenshots', 'screenshots'], ['--out', 'out'],
  ]);
  for (let i = 0; i < args.length; i++) {
    const key = names.get(args[i]);
    if (!key) {
      if (args[i] === '--help' || args[i] === '-h') return {help: true};
      throw new Error(`unknown option: ${args[i]}`);
    }
    if (i + 1 >= args.length) throw new Error(`missing value for ${args[i]}`);
    const value = args[++i];
    if (['phaseMs', 'panX', 'panY', 'panSteps', 'stepMs', 'zoomDelta', 'startZoomIn', 'overviewSteps', 'wheelStepMs'].includes(key)) {
      const number = Number(value);
      if (!Number.isFinite(number) || number < 0 || (key.endsWith('Steps') && !Number.isInteger(number))) {
        throw new Error(`invalid ${args[i - 1]} value: ${value}`);
      }
      out[key] = number;
    } else {
      out[key] = value;
    }
  }
  if (out.phaseMs < 1000) throw new Error('--phase-ms must be at least 1000');
  if (out.panSteps < 1 || out.overviewSteps < 1) throw new Error('pan and overview steps must be positive');
  if (!Number.isInteger(out.startZoomIn)) throw new Error('--start-zoom-in must be a non-negative integer');
  const overviewSetupMs = out.overviewSteps * out.wheelStepMs + out.panSteps * out.stepMs + out.wheelStepMs;
  if (out.phaseMs < overviewSetupMs) {
    throw new Error(`--phase-ms must be at least ${overviewSetupMs} to leave time for overview panning`);
  }
  return out;
}

const help = `Usage: node tools/terrain-web-bench.mjs [options]

Runs three equal-duration samples against an already-open dashboard map:
idle, repeated warm pan/zoom, and overview panning while zoomed out. It restores
the starting zoom and pan when possible; refresh/reopen if interrupted.
Use --start-zoom-in to prepare a zoomed-in image-tile view; the harness waits
for a map-pane drawImage as a basic image-rendering check before sampling.

Options:
  --cdp URL                Chrome /json endpoint (default ${defaults.cdp})
  --page TEXT              Page URL substring (default ${defaults.page})
  --canvas SELECTOR        Input canvas selector; defaults to overlay, falling back to canvas (${defaults.canvas})
  --phase-ms MS            Sample duration per phase (default ${defaults.phaseMs})
  --pan-x PX --pan-y PX    Drag distance (default ${defaults.panX},${defaults.panY})
  --pan-steps N            Drag segments (default ${defaults.panSteps})
  --step-ms MS             Delay between drag segments (default ${defaults.stepMs})
  --zoom-delta N           Warm zoom in/out amount (default ${defaults.zoomDelta})
  --start-zoom-in N        Prepare with N zoom-in wheel steps before samples (default ${defaults.startZoomIn})
  --overview-steps N       Zoom-out wheel events (default ${defaults.overviewSteps})
  --wheel-step-ms MS       Delay between overview wheel events (default ${defaults.wheelStepMs})
  --screenshots DIR        Save one PNG per measured phase
  --out FILE               JSON result path (default: /tmp/terrain-web-bench-<time>.json)
`;

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
const percentile = (sorted, p) => sorted.length ? sorted[Math.min(sorted.length - 1, Math.ceil(p * sorted.length) - 1)] : null;
const round = n => Math.round(n * 100) / 100;

function distribution(values) {
  const sorted = [...values].sort((a, b) => a - b);
  return {
    count: values.length,
    meanMs: values.length ? round(values.reduce((a, b) => a + b, 0) / values.length) : null,
    p50Ms: percentile(sorted, 0.50) === null ? null : round(percentile(sorted, 0.50)),
    p90Ms: percentile(sorted, 0.90) === null ? null : round(percentile(sorted, 0.90)),
    p95Ms: percentile(sorted, 0.95) === null ? null : round(percentile(sorted, 0.95)),
    p99Ms: percentile(sorted, 0.99) === null ? null : round(percentile(sorted, 0.99)),
    maxMs: sorted.length ? round(sorted.at(-1)) : null,
    over33Ms: values.filter(n => n > 33.3).length,
    over50Ms: values.filter(n => n > 50).length,
    over100Ms: values.filter(n => n > 100).length,
  };
}

function counterDelta(start, end) {
  const draws = {};
  for (const [name, value] of Object.entries(end.draws)) {
    const before = start.draws[name] ?? {calls: 0, ms: 0};
    draws[name] = {calls: value.calls - before.calls, totalMs: round(value.ms - before.ms)};
  }
  return {
    draws,
    canvasSizeWrites: end.canvasSizeWrites - start.canvasSizeWrites,
    canvasCount: end.canvasCount,
    canvasDimensions: end.canvasDimensions,
  };
}

function safeName(text) {
  return text.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');
}

async function main() {
  const opt = options(process.argv.slice(2));
  if (opt.help) {
    process.stdout.write(help);
    return;
  }

  const targetsResponse = await fetch(opt.cdp);
  if (!targetsResponse.ok) throw new Error(`CDP target list returned HTTP ${targetsResponse.status}: ${opt.cdp}`);
  const targets = await targetsResponse.json();
  const page = targets.find(target => target.type === 'page' && target.url.includes(opt.page));
  if (!page) {
    const pages = targets.filter(target => target.type === 'page').map(target => target.url);
    throw new Error(`no page URL contains ${JSON.stringify(opt.page)}. Open the map first. Pages: ${pages.join(' | ') || '(none)'}`);
  }

  const ws = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    ws.addEventListener('open', resolve, {once: true});
    ws.addEventListener('error', reject, {once: true});
  });

  let commandId = 0;
  const pending = new Map();
  const tileRequests = [];
  let closed = false;
  ws.addEventListener('message', ({data}) => {
    const message = JSON.parse(data);
    if (message.method === 'Network.requestWillBeSent') {
      const url = message.params.request.url;
      if (/\/api\/(?:tile\/|tiles\/)/.test(url)) {
        tileRequests.push({url, type: message.params.type ?? null, timestamp: message.params.timestamp});
      }
    }
    if (message.id) {
      const waiter = pending.get(message.id);
      if (!waiter) return;
      pending.delete(message.id);
      if (message.error) waiter.reject(new Error(message.error.message));
      else waiter.resolve(message.result);
    }
  });
  ws.addEventListener('close', () => {
    closed = true;
    for (const waiter of pending.values()) waiter.reject(new Error('CDP connection closed'));
    pending.clear();
  });

  const send = (method, params = {}) => new Promise((resolve, reject) => {
    const id = ++commandId;
    pending.set(id, {resolve, reject});
    ws.send(JSON.stringify({id, method, params}));
  });
  const evaluate = async expression => {
    const result = await send('Runtime.evaluate', {expression, returnByValue: true, awaitPromise: true});
    if (result.exceptionDetails) throw new Error(result.exceptionDetails.text ?? JSON.stringify(result.exceptionDetails));
    return result.result.value;
  };

  await send('Runtime.enable');
  await send('Page.enable');
  await send('Network.enable');
  await send('Performance.enable');
  const install = await evaluate(`(() => {
    const targetCanvas = document.querySelector(${JSON.stringify(opt.canvas)}) ||
      (${JSON.stringify(opt.canvas)} === 'canvas.map-overlay' ? document.querySelector('canvas') : null);
    if (!targetCanvas) throw new Error('canvas selector did not match: ' + ${JSON.stringify(opt.canvas)});
    if (document.visibilityState !== 'visible') throw new Error('selected page is not visible; foreground it before benchmarking');
    if (window.__terrainWebBenchLayers) return window.__terrainWebBenchLayers.ready;
    const mapPane = targetCanvas.parentElement;
    const layers = new Map();
    const state = window.__terrainWebBenchLayers = {frames: [], longTasks: [], canvasSizeWrites: 0,
      ready: null, mapPane, mapCanvas:targetCanvas};
    const registerCanvas = canvas => {
      if (layers.has(canvas)) return layers.get(canvas);
      const index = layers.size;
      const identity = [canvas.id && '#' + canvas.id, canvas.className && '.' + String(canvas.className).trim().replace(/\\s+/g,'.')]
        .filter(Boolean).join('') || 'canvas';
      const record = {index, identity, layer: canvas.dataset.layer ?? canvas.dataset.terrainLayer ?? null,
        className: String(canvas.className),
        canvas, canvasSizeWrites: 0,
        draws: {clearRect:{calls:0,ms:0}, fillRect:{calls:0,ms:0}, drawImage:{calls:0,ms:0}}};
      layers.set(canvas, record);
      return record;
    };
    const scanCanvases = () => mapPane.querySelectorAll('canvas').forEach(registerCanvas);
    scanCanvases();
    new MutationObserver(records => {
      for (const record of records) for (const node of record.addedNodes) {
        if (node instanceof HTMLCanvasElement) registerCanvas(node);
        if (node.querySelectorAll) node.querySelectorAll('canvas').forEach(registerCanvas);
      }
    }).observe(mapPane, {childList:true, subtree:true});
    const drawNames = ['clearRect','fillRect','drawImage'];
    for (const name of drawNames) {
      const proto = CanvasRenderingContext2D.prototype;
      const original = proto[name];
      proto[name] = function(...args) {
        const start = performance.now();
        try { return original.apply(this, args); }
        finally { const canvas = layers.get(this.canvas); if (canvas) { const item = canvas.draws[name]; item.calls++; item.ms += performance.now() - start; } }
      };
    }
    for (const name of ['width','height']) {
      const proto = HTMLCanvasElement.prototype;
      const descriptor = Object.getOwnPropertyDescriptor(proto, name);
      Object.defineProperty(proto, name, {configurable: descriptor.configurable, enumerable: descriptor.enumerable,
        get() { return descriptor.get.call(this); },
        set(value) { if (mapPane.contains(this)) { const canvas = registerCanvas(this); canvas.canvasSizeWrites++; state.canvasSizeWrites++; } return descriptor.set.call(this, value); }});
    }
    const snapshotLayers = () => [...layers.values()].map(({index,identity,layer,className,canvas,canvasSizeWrites,draws}) => {
      const rect = canvas.getBoundingClientRect();
      return {index,identity,layer,className,dimensions:[canvas.width,canvas.height],cssSize:[Math.round(rect.width),Math.round(rect.height)],
        sizeWrites:canvasSizeWrites,draws:Object.fromEntries(drawNames.map(name => [name,{calls:draws[name].calls,ms:draws[name].ms}]))};
    });
    const aggregateDraws = snapshots => Object.fromEntries(drawNames.map(name => [name, snapshots.reduce((sum,layer) => {
      sum.calls += layer.draws[name].calls; sum.ms += layer.draws[name].ms; return sum;
    }, {calls:0,ms:0})]));
    if (window.PerformanceObserver && PerformanceObserver.supportedEntryTypes.includes('longtask')) {
      new PerformanceObserver(list => state.longTasks.push(...list.getEntries().map(entry => entry.duration)))
        .observe({type:'longtask', buffered:true});
    }
    let previous = null;
    const sampleFrame = now => { if (previous !== null) state.frames.push(now - previous); previous = now; requestAnimationFrame(sampleFrame); };
    requestAnimationFrame(sampleFrame);
    state.mark = () => {
      scanCanvases();
      const canvasLayers = snapshotLayers();
      return {at:performance.now(), frameIndex:state.frames.length, longIndex:state.longTasks.length,
        canvasSizeWrites:state.canvasSizeWrites, canvasLayers, draws:aggregateDraws(canvasLayers),
        canvasCount:canvasLayers.length, canvasDimensions:[targetCanvas.width,targetCanvas.height]};
    };
    state.slice = (start,end) => ({
      durationMs:end.at-start.at,
      frameIntervalsMs:state.frames.slice(start.frameIndex,end.frameIndex).map(x=>Math.round(x*100)/100),
      longTasksMs:state.longTasks.slice(start.longIndex,end.longIndex).map(x=>Math.round(x*100)/100),
      canvasSizeWrites:end.canvasSizeWrites-start.canvasSizeWrites,
      draws:Object.fromEntries(Object.entries(end.draws).map(([k,v])=>[k,{calls:v.calls-start.draws[k].calls,totalMs:Math.round((v.ms-start.draws[k].ms)*100)/100}])),
      canvasLayers:end.canvasLayers.map(layer => {
        const before = start.canvasLayers.find(candidate => candidate.index === layer.index);
        const prior = before ?? {sizeWrites:0,draws:Object.fromEntries(drawNames.map(name => [name,{calls:0,ms:0}]))};
        return {...layer, sizeWrites:layer.sizeWrites-prior.sizeWrites,
          draws:Object.fromEntries(drawNames.map(name => [name,{calls:layer.draws[name].calls-prior.draws[name].calls,
            totalMs:Math.round((layer.draws[name].ms-prior.draws[name].ms)*100)/100}]))};
      }),
      canvasCount:end.canvasCount, canvasDimensions:end.canvasDimensions,
    });
    state.ready = {visibility:document.visibilityState, devicePixelRatio:devicePixelRatio,
      viewport:{width:innerWidth,height:innerHeight},selector:${JSON.stringify(opt.canvas)}};
    return state.ready;
  })()`);

  const rect = await evaluate(`(() => { const r=window.__terrainWebBenchLayers.mapCanvas.getBoundingClientRect(); return {x:r.x,y:r.y,w:r.width,h:r.height}; })()`);
  if (!rect.w || !rect.h) throw new Error(`selected canvas has no visible area: ${JSON.stringify(rect)}`);
  const pointer = {x: rect.x + rect.w / 2, y: rect.y + rect.h / 2};
  const phaseResults = [];

  const screenshot = async name => {
    if (!opt.screenshots) return null;
    await fs.mkdir(opt.screenshots, {recursive: true});
    const image = await send('Page.captureScreenshot', {format: 'png', captureBeyondViewport: false});
    const file = path.join(opt.screenshots, `${safeName(name)}.png`);
    await fs.writeFile(file, Buffer.from(image.data, 'base64'));
    return file;
  };

  const samplePhase = async (name, action = async () => {}) => {
    const metricsStart = await send('Performance.getMetrics');
    const start = await evaluate('window.__terrainWebBenchLayers.mark()');
    const requestStart = tileRequests.length;
    const deadline = Date.now() + opt.phaseMs;
    const actionResult = await action(deadline);
    const elapsed = Date.now() - (deadline - opt.phaseMs);
    await sleep(Math.max(0, opt.phaseMs - elapsed));
    const end = await evaluate('window.__terrainWebBenchLayers.mark()');
    const metricsEnd = await send('Performance.getMetrics');
    const raw = await evaluate(`window.__terrainWebBenchLayers.slice(${JSON.stringify(start)}, ${JSON.stringify(end)})`);
    const beforeMetrics = new Map(metricsStart.metrics.map(metric => [metric.name, metric.value]));
    const afterMetrics = new Map(metricsEnd.metrics.map(metric => [metric.name, metric.value]));
    const metricMs = name => beforeMetrics.has(name) && afterMetrics.has(name)
      ? round((afterMetrics.get(name) - beforeMetrics.get(name)) * 1000)
      : null;
    const result = {
      name,
      durationMs: round(raw.durationMs),
      action: actionResult ?? null,
      browserWorkMs: {
        taskDuration: metricMs('TaskDuration'),
        scriptDuration: metricMs('ScriptDuration'),
        layoutDuration: metricMs('LayoutDuration'),
      },
      frames: distribution(raw.frameIntervalsMs),
      longTasks: distribution(raw.longTasksMs),
      canvas: {
        count: raw.canvasCount,
        dimensions: raw.canvasDimensions,
        sizeWrites: raw.canvasSizeWrites,
        draws: raw.draws,
        paneTotals: {sizeWrites: raw.canvasSizeWrites, draws: raw.draws},
        layers: raw.canvasLayers,
      },
      tileRequests: tileRequests.length - requestStart,
      tileUrls: tileRequests.slice(requestStart).map(request => request.url),
      screenshot: await screenshot(name),
    };
    phaseResults.push(result);
    return result;
  };

  const mouse = (type, x, y, extra = {}) => send('Input.dispatchMouseEvent', {type, x, y, ...extra});
  const wheel = (deltaY, deltaX = 0) => mouse('mouseWheel', pointer.x, pointer.y, {deltaY, deltaX});
  const viewChange = {zoomOutSteps: 0, prepZoomInSteps: 0, netPanX: 0, netPanY: 0};
  const drag = async (dx, dy) => {
    await mouse('mouseMoved', pointer.x, pointer.y);
    await mouse('mousePressed', pointer.x, pointer.y, {button: 'left', buttons: 1, clickCount: 1});
    for (let i = 1; i <= opt.panSteps; i++) {
      await mouse('mouseMoved', pointer.x + dx * i / opt.panSteps,
        pointer.y + dy * i / opt.panSteps, {button: 'left', buttons: 1});
      await sleep(opt.stepMs);
    }
    await mouse('mouseReleased', pointer.x + dx, pointer.y + dy, {button: 'left', buttons: 0});
  };
  const trackedDrag = async (dx, dy) => {
    await drag(dx, dy);
    viewChange.netPanX += dx;
    viewChange.netPanY += dy;
  };
  const undoPan = async () => {
    if (viewChange.netPanX || viewChange.netPanY) {
      await drag(-viewChange.netPanX, -viewChange.netPanY);
      viewChange.netPanX = 0;
      viewChange.netPanY = 0;
    }
  };
  const warmInteraction = async deadline => {
    let direction = 1;
    let completedCycles = 0;
    const cycleMs = opt.panSteps * opt.stepMs + 2 * opt.stepMs;
    while (Date.now() + cycleMs < deadline) {
      const dx = direction * opt.panX;
      const dy = direction * opt.panY;
      await trackedDrag(dx, dy);
      await wheel(-opt.zoomDelta);
      await sleep(opt.stepMs);
      await wheel(opt.zoomDelta);
      direction *= -1;
      completedCycles++;
    }
    return {completedCycles, netPanX: viewChange.netPanX, netPanY: viewChange.netPanY};
  };
  const overviewInteraction = async deadline => {
    for (let i = 0; i < opt.overviewSteps; i++) {
      await wheel(120);
      viewChange.zoomOutSteps++;
      await sleep(opt.wheelStepMs);
    }
    let direction = 1;
    let completedPanCycles = 0;
    const cycleMs = opt.panSteps * opt.stepMs + opt.wheelStepMs;
    while (Date.now() + cycleMs < deadline) {
      const dx = direction * opt.panX;
      const dy = direction * opt.panY;
      await trackedDrag(dx, dy);
      await sleep(opt.wheelStepMs);
      direction *= -1;
      completedPanCycles++;
    }
    return {zoomOutEvents: viewChange.zoomOutSteps, completedPanCycles,
      netPanX: viewChange.netPanX, netPanY: viewChange.netPanY};
  };
  const restoreView = async () => {
    await undoPan();
    for (let i = 0; i < viewChange.zoomOutSteps; i++) {
      await wheel(-120);
      await sleep(opt.wheelStepMs);
    }
    viewChange.zoomOutSteps = 0;
    for (let i = 0; i < viewChange.prepZoomInSteps; i++) {
      await wheel(120);
      await sleep(opt.wheelStepMs);
    }
    viewChange.prepZoomInSteps = 0;
  };

  const originalView = await evaluate(`(() => ({url:location.href, title:document.title, visibility:document.visibilityState,
    devicePixelRatio, viewport:{width:innerWidth,height:innerHeight}, mapCanvas:(()=>{const r=window.__terrainWebBenchLayers.mapCanvas.getBoundingClientRect(); return {x:r.x,y:r.y,width:r.width,height:r.height}})()}))()`);
  let preparation = {zoomInEvents: 0, mapDrawImageCalls: 0, imageObserved: false};
  try {
    for (let i = 0; i < opt.startZoomIn; i++) {
      await wheel(-120);
      viewChange.prepZoomInSteps++;
      await sleep(opt.wheelStepMs);
    }
    if (opt.startZoomIn) {
      const deadline = Date.now() + 30000;
      while (Date.now() < deadline) {
        preparation.mapDrawImageCalls = await evaluate(`window.__terrainWebBenchLayers.mark().draws.drawImage.calls`);
        if (preparation.mapDrawImageCalls > 0) { preparation.imageObserved = true; break; }
        await sleep(200);
      }
      preparation.zoomInEvents = viewChange.prepZoomInSteps;
      if (!preparation.imageObserved) {
        preparation.diagnosticScreenshot = await screenshot('preparation-failed');
        throw new Error(`no terrain drawImage observed after ${preparation.zoomInEvents} --start-zoom-in steps; tile requests=${JSON.stringify(tileRequests.map(request => request.url))}; refusing a close-terrain benchmark without observed image drawing`);
      }
      await sleep(1500);
    }
    await samplePhase('idle');
    await samplePhase('warm-pan-zoom', warmInteraction);
    await undoPan();
    await samplePhase('overview', overviewInteraction);
  } finally {
    if (!closed && (viewChange.zoomOutSteps || viewChange.prepZoomInSteps || viewChange.netPanX || viewChange.netPanY)) {
      try { await restoreView(); } catch (error) { process.stderr.write(`warning: could not restore starting view: ${error.message}\n`); }
    }
    ws.close();
  }

  const output = {
    schema: 'terrain-web-bench/v1',
    timestamp: new Date().toISOString(),
    target: {url: page.url, title: page.title},
    settings: opt,
    preparation,
    browser: install,
    page: originalView,
    phases: phaseResults,
  };
  const file = opt.out ?? path.join('/tmp', `terrain-web-bench-${Date.now()}.json`);
  await fs.writeFile(file, `${JSON.stringify(output, null, 2)}\n`);
  process.stdout.write(`${JSON.stringify({output: file, page: page.url, phases: phaseResults.map(({name, frames, longTasks, canvas, tileRequests, durationMs}) => ({
    name, durationMs, frameIntervalsMs: frames, longTasksMs: longTasks, canvas: canvas, tileRequests,
  }))}, null, 2)}\n`);
}

main().catch(error => {
  process.stderr.write(`terrain-web-bench: ${error.stack ?? error}\n`);
  process.exitCode = 1;
});
