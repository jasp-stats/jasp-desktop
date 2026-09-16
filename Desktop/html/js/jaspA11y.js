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
	}
};
