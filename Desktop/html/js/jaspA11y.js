// Accessibility support for the results page.
// Enriches the rendered Backbone views with roles/labels that screen
// readers (Narrator, VoiceOver) can narrate, and hosts the keyboard
// navigation helpers. All enrichment is idempotent: analyses re-render
// on every result update and are re-enriched from scratch.

JASPWidgets.a11y = {

	// Nearest ancestor container that carries a visible title.
	// Containers are objectViews (.jasp-collapsible) whose toolbar heading
	// (.in-toolbar) has text; hidden collections (empty title) are skipped.
	resolveContainer: function (el) {
		var node = $(el).closest('.jasp-collapsible');
		while (node.length) {
			var heading = node.children('.jasp-toolbar').first().find('.in-toolbar').first().text().trim();
			if (heading !== '')
				return { el: node, title: heading };
			node = node.parent().closest('.jasp-collapsible');
		}
		return { el: null, title: '' };
	},

	// Label for a plot: own title, else container title (+ N of M), else
	// analysis title (+ N of M), else bare 'Plot'.
	plotLabel: function (plotEl, index, total, containerTitle) {
		var own = plotEl.getAttribute('data-plot-title');
		if (own && own.trim() !== '')
			return 'Plot: ' + own.trim();

		var base = containerTitle && containerTitle.trim() !== '' ? containerTitle.trim() : '';
		if (base !== '') {
			if (total > 1)
				return 'Plot: ' + base + ', plot ' + (index + 1) + ' of ' + total;
			return 'Plot: ' + base;
		}
		if (total > 1)
			return 'Plot, ' + (index + 1) + ' of ' + total;
		return 'Plot';
	},

	// Give every plot under root a role="img" + computed aria-label.
	// Plots render as CSS background-image divs, which are invisible to
	// screen readers without this. Position/grouping is resolved against
	// the attached DOM, so call this after the analysis tree is in place.
	enrichPlots: function (root, analysisTitle) {

		var plots = $(root).find('.jasp-image-image').filter(function () {
			return this.hasAttribute('data-plot-title');
		});
		if (plots.length === 0)
			return;

		// Group plots by their resolved titled container so that N-of-M
		// is counted among visually sibling plots only.
		var groups		= [];
		var groupMembers	= [];
		plots.each(function () {
			var resolved = JASPWidgets.a11y.resolveContainer(this);
			var groupEl	= resolved.el === null ? (root[0] || root) : resolved.el[0];
			var title	= resolved.title !== '' ? resolved.title : (analysisTitle || '');

			var gi = groups.indexOf(groupEl);
			if (gi === -1) {
				groups.push(groupEl);
				groupMembers.push([]);
				gi = groups.length - 1;
			}
			groupMembers[gi].push({ plot: this, containerTitle: title });
		});

		for (var g = 0; g < groups.length; g++) {
			var members = groupMembers[g];
			for (var i = 0; i < members.length; i++) {
				var el = members[i].plot;
				el.setAttribute('role', 'img');
				el.setAttribute('tabindex', '0');
				el.setAttribute('aria-label', JASPWidgets.a11y.plotLabel(el, i, members.length, members[i].containerTitle));
			}
		}
	},

	// ── Keyboard navigation engine ──────────────────────────────────────
	//
	// Arrow Up/Down move focus between the narratable blocks of the
	// results (titles, tables, plots, notes, markdown, error boxes) in
	// document order. Enter activates the block under focus (table:
	// drill into cell navigation, collapsible container: toggle, note:
	// edit). Shift+Enter opens the block's context menu (also
	// Ctrl+Enter and Shift+F10). Arrows are ignored while a text editor
	// has focus.

	_blocksSelector: '.in-toolbar, table[role="table"], .jasp-image-image[data-plot-title], .jasp-notes, .jasp-md-text, .error-message-box',

	visibleBlocks: function () {
		var blocks = [];
		var els = document.querySelectorAll(JASPWidgets.a11y._blocksSelector);
		for (var i = 0; i < els.length; i++) {
			var el = els[i];
			if (el.classList.contains('jasp-hide'))
				continue;
			if (el.offsetParent === null && el !== document.activeElement)
				continue; // hidden (display:none)
			blocks.push(el);
		}
		return blocks;
	},

	isEditingContext: function (el) {
		return !!(el && el.closest && el.closest('input, textarea, select, [contenteditable="true"], .ql-editor'));
	},

	blockMove: function (dir) {
		var a = JASPWidgets.a11y;

		// leaving a table drill-in anchors the next move at the table
		var current = a.drillTable || document.activeElement;
		a.exitDrill();

		var nav = a.visibleBlocks();
		if (nav.length === 0)
			return;

		var next = null;
		if (!current || current === document.body) {
			next = dir > 0 ? nav[0] : nav[nav.length - 1];
		} else {
			var candidates = [];
			for (var j = 0; j < nav.length; j++) {
				var rel = current.compareDocumentPosition(nav[j]);
				if (dir > 0 && (rel & Node.DOCUMENT_POSITION_FOLLOWING))
					candidates.push(nav[j]);
				else if (dir < 0 && (rel & Node.DOCUMENT_POSITION_PRECEDING))
					candidates.push(nav[j]);
			}
			next = dir > 0 ? candidates[0] : candidates[candidates.length - 1];
		}
		if (next)
			next.focus();
	},

	// The title element whose context menu governs this block: the
	// nearest ancestor subtree containing an .in-toolbar title.
	_blockMenuAnchor: function (el) {
		var root = el;
		while (root && root !== document.body) {
			if (root.querySelectorAll) {
				var titles = root.querySelectorAll('.in-toolbar');
				if (titles.length > 0)
					return titles[titles.length - 1];
			}
			root = root.parentElement;
		}
		return null;
	},

	// Opens the context menu governing the given block (table, plot,
	// note, ...). Reuses the Toolbar's own keydown handling via a
	// jQuery-synthesized Shift+Enter on the anchor title, so the menu
	// anchors to that title element exactly like a mouse activation.
	openMenuFor: function (el) {
		var anchor = JASPWidgets.a11y._blockMenuAnchor(el);
		if (!anchor)
			return;
		$(anchor).trigger({
			type: 'keydown', which: 13, keyCode: 13, key: 'Enter',
			shiftKey: true, target: anchor
		});
	},

	// ── AT activation support ───────────────────────────────────────────
	// VoiceOver invokes AXPress as a Blink default action. Qt's generic
	// accessibility bridge can expose that as an AXPress action, but JASP
	// still needs DOM activation handlers. Real mouse clicks remain trusted;
	// accessibility-simulated clicks are untrusted, so we can support both
	// without hijacking normal table interaction.

	_activationSelector: 'table[role="table"], .in-toolbar, .jasp-notes',
	_cellRolesSelector: '[role="rowheader"], [role="columnheader"], [role="gridcell"]',

	// Blink marks accessibility-simulated clicks as trusted, but it does not
	// attach an input-device sourceCapabilities object to them. Real mouse,
	// touch, and pen clicks do have sourceCapabilities. If a browser/engine
	// does not expose that property, fall back to the older untrusted-click
	// signal so we never hijack a normal pointer click.
	_isATActivationClick: function (event) {
		if (!event)
			return false;
		if (!('sourceCapabilities' in event))
			return event.isTrusted === false;
		return !event.sourceCapabilities;
	},

	activateElement: function (el) {
		var a = JASPWidgets.a11y;
		if (!el || !el.closest)
			return false;

		var table = el.closest('table[role="table"]');
		if (table) {
			a.exitDrill();
			var cell = el.matches && el.matches(a._cellRolesSelector) ? el : null;
			a.drillCells(table, cell);
			return true;
		}

		if (el.matches && el.matches('.in-toolbar')) {
			$(el).trigger({
				type: 'keydown', which: 13, keyCode: 13, key: 'Enter', target: el
			});
			return true;
		}

		var note = el.closest('.jasp-notes');
		if (note) {
			$(note).trigger({
				type: 'keydown', which: 13, keyCode: 13, key: 'Enter', target: note
			});
			return true;
		}

		return false;
	},

	_bindActivationHandler: function (el) {
		if (!el || el.dataset.jaspA11yActivated === '1')
			return;

		el.dataset.jaspA11yActivated = '1';
		el.addEventListener('click', function (event) {
			if (!JASPWidgets.a11y._isATActivationClick(event))
				return;
			JASPWidgets.a11y.activateElement(event.target && event.target.closest ? event.target : event.currentTarget);
			event.preventDefault();
		});
	},

	enrichActions: function (root) {
		var rootEl = root && root.length ? root[0] : root;
		if (!rootEl || !rootEl.querySelectorAll)
			return;

		var els = rootEl.querySelectorAll(JASPWidgets.a11y._activationSelector);
		for (var i = 0; i < els.length; i++) {
			if (!els[i].classList.contains('jasp-hide'))
				JASPWidgets.a11y._bindActivationHandler(els[i]);
		}

		// Make cells programmatically focusable without adding every cell to
		// the Tab order. VoiceOver needs to be able to place focus on a table
		// cell after it has entered the results area.
		var tables = rootEl.querySelectorAll('table[role="table"]');
		for (var t = 0; t < tables.length; t++) {
			var cellEls = tables[t].querySelectorAll(JASPWidgets.a11y._cellRolesSelector);
			for (var c = 0; c < cellEls.length; c++) {
				if (cellEls[c].tabIndex < 0)
					cellEls[c].tabIndex = -1;
			}
		}
	},

	// ── table cell drill-in ─────────────────────────────────────────────

	drillTable: null,
	_drillGrid: null,
	drillPos: null,

	_normalizeCellText: function (cell) {
		if (!cell)
			return '';
		var text = cell.getAttribute('aria-label') || cell.textContent || '';
		return text.replace(/\u00a0/g, ' ').replace(/&nbsp;/gi, ' ').trim();
	},

	_visibleCellText: function (cell) {
		if (!cell)
			return '';
		var text = cell.textContent || '';
		return text.replace(/\u00a0/g, ' ').replace(/&nbsp;/gi, ' ').trim();
	},

	_tableGrid: function (table) {
		var grid = [];
		var rows = table.querySelectorAll('tr');
		for (var i = 0; i < rows.length; i++) {
			var rowCells = [];
			var cells = rows[i].querySelectorAll('[role="rowheader"], [role="columnheader"], [role="gridcell"]');
			for (var j = 0; j < cells.length; j++) {
				// Skip invisible spacers; VoiceOver gets stuck on them and
				// announces nothing useful when the drill starts there.
				if (JASPWidgets.a11y._normalizeCellText(cells[j]) !== '')
					rowCells.push(cells[j]);
			}
			if (rowCells.length > 0)
				grid.push(rowCells);
		}
		return grid;
	},

	drillCells: function (table, startCell) {
		var a = JASPWidgets.a11y;
		a._drillGrid = a._tableGrid(table);
		if (a._drillGrid.length === 0)
			return;

		var pos = { r: 0, c: 0 };
		if (startCell) {
			var found = false;
			for (var r = 0; r < a._drillGrid.length && !found; r++) {
				for (var c = 0; c < a._drillGrid[r].length; c++) {
					if (a._drillGrid[r][c] === startCell) {
						pos = { r: r, c: c };
						found = true;
						break;
					}
				}
			}
		} else {
			// Prefer the first cell with readable content so pressing Enter
			// on a table does not land on empty header spacers.
			var foundVisible = false;
			for (var r = 0; r < a._drillGrid.length && !foundVisible; r++) {
				for (var c = 0; c < a._drillGrid[r].length; c++) {
					if (a._visibleCellText(a._drillGrid[r][c]) !== '') {
						pos = { r: r, c: c };
						foundVisible = true;
						break;
					}
				}
			}
		}

		a.drillTable = table;
		a.drillPos = pos;
		a._focusDrillCell();
	},

	_focusDrillCell: function () {
		var a = JASPWidgets.a11y;
		var row = a._drillGrid[a.drillPos.r];
		var cell = row[Math.min(a.drillPos.c, row.length - 1)];
		cell.setAttribute('tabindex', '-1');
		cell.focus();
	},

	exitDrill: function () {
		var a = JASPWidgets.a11y;
		if (!a.drillTable)
			return;
		var table = a.drillTable;
		a.drillTable = null;
		a._drillGrid = null;
		a.drillPos = null;
		table.focus();
	},

	drillMove: function (dR, dC) {
		var a = JASPWidgets.a11y;
		if (!a.drillTable)
			return;
		var g = a._drillGrid;
		var r = Math.max(0, Math.min(g.length - 1, a.drillPos.r + dR));
		var c = Math.max(0, Math.min(g[r].length - 1, a.drillPos.c + dC));
		a.drillPos = { r: r, c: c };
		a._focusDrillCell();
	},

	initNav: function () {
		document.addEventListener('keydown', function (e) {
			var a = JASPWidgets.a11y;
			var target = e.target;

			// never interfere while typing or editing text
			if (a.isEditingContext(target))
				return;

			// table cell navigation (drill-in mode)
			if (a.drillTable) {
				if (e.key === 'ArrowDown')		{ e.preventDefault(); a.drillMove(1, 0);	return; }
				if (e.key === 'ArrowUp')		{ e.preventDefault(); a.drillMove(-1, 0);	return; }
				if (e.key === 'ArrowRight')		{ e.preventDefault(); a.drillMove(0, 1);	return; }
				if (e.key === 'ArrowLeft')		{ e.preventDefault(); a.drillMove(0, -1);	return; }
				if (e.key === 'Escape')			{ e.preventDefault(); a.exitDrill();		return; }
				if (e.key === 'Tab')			{ a.drillTable = null; a._drillGrid = null; a.drillPos = null; return; }
				return;
			}

			// Shift+Enter opens the block's context menu; titles handle
			// this themselves via the Toolbar keydown handler.
			if (e.key === 'Enter' && e.shiftKey) {
				if (target && target.matches && target.matches('.in-toolbar'))
					return;
				e.preventDefault();
				a.openMenuFor(target);
				return;
			}

			// Enter on a focused table drills into cell navigation
			if (e.key === 'Enter' && target && target.matches && target.matches('table[role="table"]')) {
				e.preventDefault();
				a.drillCells(target);
				return;
			}

			// block-level arrow navigation (tables included)
			if (e.key === 'ArrowDown')	{ e.preventDefault(); a.blockMove(1);	return; }
			if (e.key === 'ArrowUp')	{ e.preventDefault(); a.blockMove(-1);	return; }
		});
	}
};

// the results page is the only consumer; jQuery is loaded before this file
$(function () {
	JASPWidgets.a11y.initNav();
});
