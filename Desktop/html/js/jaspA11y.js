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
	// document order; tables are skipped at block level (Tab reaches
	// them). Enter on a focused table drills into cell navigation:
	// arrows move cell-by-cell with full header context via the headers
	// attributes, Escape/Tab returns to block navigation. Arrows are
	// ignored while a text editor has focus.

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

		// arrow navigation skips tables (Enter on a focused table drills in)
		var all = a.visibleBlocks();
		var nav = [];
		for (var i = 0; i < all.length; i++)
			if (all[i].tagName !== 'TABLE')
				nav.push(all[i]);
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

	// ── table cell drill-in ─────────────────────────────────────────────

	drillTable: null,
	_drillGrid: null,
	drillPos: null,

	_tableGrid: function (table) {
		var grid = [];
		var rows = table.querySelectorAll('tr');
		for (var i = 0; i < rows.length; i++) {
			var rowCells = rows[i].querySelectorAll('[role="rowheader"], [role="columnheader"], [role="gridcell"]');
			if (rowCells.length > 0) {
				var arr = [];
				for (var j = 0; j < rowCells.length; j++)
					arr.push(rowCells[j]);
				grid.push(arr);
			}
		}
		return grid;
	},

	drillCells: function (table) {
		var a = JASPWidgets.a11y;
		a._drillGrid = a._tableGrid(table);
		if (a._drillGrid.length === 0)
			return;
		a.drillTable = table;
		a.drillPos = { r: 0, c: 0 };
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

			// Enter on a focused table drills into cell navigation
			if (e.key === 'Enter' && target && target.matches && target.matches('table[role="table"]')) {
				e.preventDefault();
				a.drillCells(target);
				return;
			}

			// block-level arrow navigation
			if (e.key === 'ArrowDown')	{ e.preventDefault(); a.blockMove(1);	return; }
			if (e.key === 'ArrowUp')	{ e.preventDefault(); a.blockMove(-1);	return; }
		});
	}
};

// the results page is the only consumer; jQuery is loaded before this file
$(function () {
	JASPWidgets.a11y.initNav();
});
