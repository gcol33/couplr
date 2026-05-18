// =============================================================================
// lap_animate htmlwidget
// =============================================================================
// Renders an interactive bipartite-graph animation of an assignment algorithm.
// Consumes a trace produced by R/animate.R with the following shape:
//
//   x = {
//     meta: { algorithm, n_rows, n_cols, cost_matrix[][], maximize,
//             total_cost, description },
//     frames: [
//       { step, phase, description, matching: int[n_rows] (1-indexed col,
//         0 = unmatched), dual_u: number[] | null, dual_v: number[] | null,
//         active_edges: [[r,c], ...], path: [[r,c], ...] },
//       ...
//     ]
//   }
//
// Edge highlighting classes:
//   - matched : final/current matching (green)
//   - active  : currently being explored (orange)
//   - path    : on current augmenting path / shortest-path tree (red dashed)
// =============================================================================

HTMLWidgets.widget({
  name: 'lap_animate',
  type: 'output',

  factory: function (el, width, height) {

    // -----------------------------------------------------------------------
    // DOM scaffolding (built once per widget instance)
    // -----------------------------------------------------------------------
    el.classList.add('couplr-lap-animate');
    el.innerHTML = '';

    const header = document.createElement('div');
    header.className = 'couplr-header';
    header.innerHTML =
      '<div><span class="algo-name"></span><span class="phase-badge"></span></div>' +
      '<div class="step-counter"></div>';
    el.appendChild(header);

    const canvas = document.createElement('div');
    canvas.className = 'couplr-canvas';
    el.appendChild(canvas);

    const footer = document.createElement('div');
    footer.className = 'couplr-footer';
    footer.innerHTML =
      '<div class="couplr-description"></div>' +
      '<div class="couplr-controls">' +
        '<button class="reset" title="Reset to first frame">&#x23EE;</button>' +
        '<button class="step-prev" title="Step backward">&#x25C0;</button>' +
        '<button class="play-pause" title="Play / pause">Play</button>' +
        '<button class="step-next" title="Step forward">&#x25B6;</button>' +
        '<span class="speed-control">' +
          '<label>Speed</label>' +
          '<input type="range" min="100" max="2000" step="100" value="800">' +
        '</span>' +
      '</div>';
    el.appendChild(footer);

    const svgNS = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(svgNS, 'svg');
    svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
    canvas.appendChild(svg);

    // Layers (back-to-front: edges, edge labels, nodes, node labels, duals)
    const gEdges     = document.createElementNS(svgNS, 'g');
    const gEdgeCosts = document.createElementNS(svgNS, 'g');
    const gNodes     = document.createElementNS(svgNS, 'g');
    const gNodeLabels = document.createElementNS(svgNS, 'g');
    const gDuals     = document.createElementNS(svgNS, 'g');
    svg.appendChild(gEdges);
    svg.appendChild(gEdgeCosts);
    svg.appendChild(gNodes);
    svg.appendChild(gNodeLabels);
    svg.appendChild(gDuals);

    // -----------------------------------------------------------------------
    // State
    // -----------------------------------------------------------------------
    const state = {
      trace: null,
      n: 0, m: 0,
      frameIdx: 0,
      isPlaying: false,
      speedMs: 800,
      timer: null,
      layout: null,    // {nodeR, rowX, colX, ys: {rows:[], cols:[]}, w, h}
      edgeKey: {}      // map "r-c" -> SVG line element
    };

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------
    function edgeKey(r, c) { return r + '-' + c; }

    function normalizeEdgeList(edges) {
      if (!edges) return [];
      if (!Array.isArray(edges)) return [];
      const out = [];
      for (const e of edges) {
        if (Array.isArray(e) && e.length >= 2) {
          out.push([+e[0], +e[1]]);
        }
      }
      return out;
    }

    function fmtNum(x) {
      if (x === null || x === undefined || Number.isNaN(x)) return '';
      if (!Number.isFinite(x)) return '';
      if (Math.abs(x) >= 1000 || (Math.abs(x) < 0.01 && x !== 0)) {
        return x.toExponential(1);
      }
      if (Number.isInteger(x)) return String(x);
      return x.toFixed(2);
    }

    // -----------------------------------------------------------------------
    // Layout
    // -----------------------------------------------------------------------
    function computeLayout() {
      const rect = canvas.getBoundingClientRect();
      const W = Math.max(rect.width, 320);
      const H = Math.max(rect.height, 200);

      svg.setAttribute('viewBox', '0 0 ' + W + ' ' + H);
      svg.setAttribute('width', W);
      svg.setAttribute('height', H);

      const padY = 24;
      const padX = 64;
      const nMax = Math.max(state.n, state.m, 1);
      const spacing = (H - 2 * padY) / Math.max(nMax - 1, 1);
      const nodeR = Math.max(8, Math.min(22, spacing * 0.35));

      const rowX = padX;
      const colX = W - padX;

      // Center each column vertically
      function ys(count) {
        if (count <= 0) return [];
        if (count === 1) return [H / 2];
        const totalH = (count - 1) * spacing;
        const startY = (H - totalH) / 2;
        const out = [];
        for (let i = 0; i < count; i++) out.push(startY + i * spacing);
        return out;
      }

      state.layout = {
        W: W, H: H,
        rowX: rowX, colX: colX,
        nodeR: nodeR,
        rowYs: ys(state.n),
        colYs: ys(state.m)
      };
    }

    // -----------------------------------------------------------------------
    // Scene construction (nodes, all edges) — rebuilt on renderValue/resize
    // -----------------------------------------------------------------------
    function buildScene() {
      // Clear
      gEdges.innerHTML = '';
      gEdgeCosts.innerHTML = '';
      gNodes.innerHTML = '';
      gNodeLabels.innerHTML = '';
      gDuals.innerHTML = '';
      state.edgeKey = {};

      const L = state.layout;
      const cost = state.trace.meta.cost_matrix;
      const showCosts = (state.n <= 8 && state.m <= 8);

      // --- Edges (n * m) ----
      for (let i = 0; i < state.n; i++) {
        for (let j = 0; j < state.m; j++) {
          const c = (cost && cost[i] && cost[i][j] !== undefined) ? cost[i][j] : null;
          // Skip forbidden edges in rendering (null/NaN)
          if (c === null || !Number.isFinite(c)) continue;

          const line = document.createElementNS(svgNS, 'line');
          line.setAttribute('x1', L.rowX);
          line.setAttribute('y1', L.rowYs[i]);
          line.setAttribute('x2', L.colX);
          line.setAttribute('y2', L.colYs[j]);
          line.setAttribute('class', 'couplr-edge');
          gEdges.appendChild(line);
          state.edgeKey[edgeKey(i + 1, j + 1)] = line;

          if (showCosts) {
            const t = document.createElementNS(svgNS, 'text');
            const mx = (L.rowX + L.colX) / 2;
            const my = (L.rowYs[i] + L.colYs[j]) / 2;
            t.setAttribute('x', mx);
            t.setAttribute('y', my);
            t.setAttribute('class', 'couplr-edge-cost');
            t.setAttribute('text-anchor', 'middle');
            t.setAttribute('dominant-baseline', 'central');
            t.textContent = fmtNum(c);
            // Background rect for legibility
            const bg = document.createElementNS(svgNS, 'rect');
            const w = 22, h = 12;
            bg.setAttribute('x', mx - w / 2);
            bg.setAttribute('y', my - h / 2);
            bg.setAttribute('width', w);
            bg.setAttribute('height', h);
            bg.setAttribute('fill', '#FAFAFA');
            bg.setAttribute('stroke', 'none');
            bg.setAttribute('opacity', '0.85');
            gEdgeCosts.appendChild(bg);
            gEdgeCosts.appendChild(t);
          }
        }
      }

      // --- Row nodes ----
      for (let i = 0; i < state.n; i++) {
        const node = document.createElementNS(svgNS, 'circle');
        node.setAttribute('cx', L.rowX);
        node.setAttribute('cy', L.rowYs[i]);
        node.setAttribute('r', L.nodeR);
        node.setAttribute('class', 'couplr-node row');
        gNodes.appendChild(node);

        const label = document.createElementNS(svgNS, 'text');
        label.setAttribute('x', L.rowX);
        label.setAttribute('y', L.rowYs[i]);
        label.setAttribute('class', 'couplr-node-label');
        label.textContent = 'R' + (i + 1);
        gNodeLabels.appendChild(label);
      }

      // --- Col nodes ----
      for (let j = 0; j < state.m; j++) {
        const node = document.createElementNS(svgNS, 'circle');
        node.setAttribute('cx', L.colX);
        node.setAttribute('cy', L.colYs[j]);
        node.setAttribute('r', L.nodeR);
        node.setAttribute('class', 'couplr-node col');
        gNodes.appendChild(node);

        const label = document.createElementNS(svgNS, 'text');
        label.setAttribute('x', L.colX);
        label.setAttribute('y', L.colYs[j]);
        label.setAttribute('class', 'couplr-node-label');
        label.textContent = 'C' + (j + 1);
        gNodeLabels.appendChild(label);
      }
    }

    // -----------------------------------------------------------------------
    // Frame rendering: drive edge classes + dual labels + header/footer text
    // -----------------------------------------------------------------------
    function renderFrame(idx) {
      const trace = state.trace;
      if (!trace) return;
      const nFrames = trace.frames.length;
      if (idx < 0) idx = 0;
      if (idx >= nFrames) idx = nFrames - 1;
      state.frameIdx = idx;

      const frame = trace.frames[idx];

      // Reset every edge to default
      for (const k in state.edgeKey) {
        state.edgeKey[k].setAttribute('class', 'couplr-edge');
      }

      // Matched edges from frame.matching (row -> col, 1-indexed, 0 = unmatched)
      if (Array.isArray(frame.matching)) {
        for (let i = 0; i < frame.matching.length; i++) {
          const c = frame.matching[i];
          if (c && c > 0) {
            const line = state.edgeKey[edgeKey(i + 1, c)];
            if (line) line.setAttribute('class', 'couplr-edge matched');
          }
        }
      }

      // Active edges
      const active = normalizeEdgeList(frame.active_edges);
      for (const [r, c] of active) {
        const line = state.edgeKey[edgeKey(r, c)];
        if (line) line.setAttribute('class', 'couplr-edge active');
      }

      // Path edges (drawn last so they win over matched/active)
      const path = normalizeEdgeList(frame.path);
      for (const [r, c] of path) {
        const line = state.edgeKey[edgeKey(r, c)];
        if (line) line.setAttribute('class', 'couplr-edge path');
      }

      // Dual labels
      gDuals.innerHTML = '';
      const L = state.layout;
      const ur = frame.dual_u, vc = frame.dual_v;
      if (Array.isArray(ur)) {
        for (let i = 0; i < ur.length && i < state.n; i++) {
          if (ur[i] === null || ur[i] === undefined) continue;
          const t = document.createElementNS(svgNS, 'text');
          t.setAttribute('x', L.rowX - L.nodeR - 6);
          t.setAttribute('y', L.rowYs[i]);
          t.setAttribute('class', 'couplr-dual-label');
          t.setAttribute('text-anchor', 'end');
          t.setAttribute('dominant-baseline', 'central');
          t.textContent = 'u=' + fmtNum(ur[i]);
          gDuals.appendChild(t);
        }
      }
      if (Array.isArray(vc)) {
        for (let j = 0; j < vc.length && j < state.m; j++) {
          if (vc[j] === null || vc[j] === undefined) continue;
          const t = document.createElementNS(svgNS, 'text');
          t.setAttribute('x', L.colX + L.nodeR + 6);
          t.setAttribute('y', L.colYs[j]);
          t.setAttribute('class', 'couplr-dual-label');
          t.setAttribute('text-anchor', 'start');
          t.setAttribute('dominant-baseline', 'central');
          t.textContent = 'v=' + fmtNum(vc[j]);
          gDuals.appendChild(t);
        }
      }

      // Header / footer text
      header.querySelector('.algo-name').textContent = trace.meta.algorithm || '';
      header.querySelector('.phase-badge').textContent = frame.phase || '';
      header.querySelector('.step-counter').textContent =
        'Step ' + (idx + 1) + ' / ' + nFrames;
      footer.querySelector('.couplr-description').textContent =
        frame.description || '';

      // Disable step buttons at boundaries
      footer.querySelector('.step-prev').disabled = (idx <= 0);
      footer.querySelector('.step-next').disabled = (idx >= nFrames - 1);
    }

    // -----------------------------------------------------------------------
    // Playback
    // -----------------------------------------------------------------------
    function setPlaying(playing) {
      state.isPlaying = playing;
      footer.querySelector('.play-pause').textContent = playing ? 'Pause' : 'Play';
      if (state.timer) { clearInterval(state.timer); state.timer = null; }
      if (playing) {
        state.timer = setInterval(() => {
          if (state.frameIdx >= state.trace.frames.length - 1) {
            setPlaying(false);
            return;
          }
          renderFrame(state.frameIdx + 1);
        }, state.speedMs);
      }
    }

    // -----------------------------------------------------------------------
    // Control wiring
    // -----------------------------------------------------------------------
    footer.querySelector('.play-pause').addEventListener('click', () => {
      if (!state.trace) return;
      if (state.frameIdx >= state.trace.frames.length - 1 && !state.isPlaying) {
        renderFrame(0);
      }
      setPlaying(!state.isPlaying);
    });
    footer.querySelector('.step-prev').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(state.frameIdx - 1);
    });
    footer.querySelector('.step-next').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(state.frameIdx + 1);
    });
    footer.querySelector('.reset').addEventListener('click', () => {
      setPlaying(false);
      renderFrame(0);
    });
    footer.querySelector('.speed-control input').addEventListener('input', (ev) => {
      // Higher value = slower (ms per frame). Invert slider feel:
      // slider is min=100 (fast) max=2000 (slow); use directly as ms.
      state.speedMs = +ev.target.value;
      if (state.isPlaying) setPlaying(true); // restart with new speed
    });

    // -----------------------------------------------------------------------
    // htmlwidgets interface
    // -----------------------------------------------------------------------
    return {

      renderValue: function (x) {
        state.trace = x;
        state.n = x.meta.n_rows || 0;
        state.m = x.meta.n_cols || 0;
        state.frameIdx = 0;
        setPlaying(false);

        computeLayout();
        buildScene();
        renderFrame(0);
      },

      resize: function (w, h) {
        if (!state.trace) return;
        computeLayout();
        buildScene();
        renderFrame(state.frameIdx);
      }

    };
  }
});
