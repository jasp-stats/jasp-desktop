JASPWidgets.htmlNode = Backbone.Model.extend({

	defaults: {
		title: "",
		citation: null,
        rawtext: "",
		text: "",
		class: "",
		maxWidth: "10cm",
		elementType: "p"
	}
});


convertModelToHtml = function(model)
{
	var optText			= model.get("text");
	var optClass		= model.get("class");
	var optMaxWidth		= model.get("maxWidth");
	var optElementType	= model.get("elementType");

	if(optElementType === undefined || optElementType === null)
		optElementType = "p";

	if(optClass === undefined || optClass === null)
		optClass = "";

	if(optText === undefined || optText === null)
		optText = "";

	var html = '<span style="max-width:'+optMaxWidth+'; display:block;">';
	if(optElementType === "errorMsg")	html = '<div class="fatalError analysis-error-message error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"/>' + optText + '</div>'
	else if(optElementType === "")		html +=														optText + "</span>";
	else if(optClass === "")			html += '<'+ optElementType +' >' +							optText + '</'+ optElementType +'></span>';
	else								html += '<'+ optElementType +' class="'+ optClass +'">' +	optText + '</'+ optElementType +'></span>';

	return html;
}

JASPWidgets.htmlNodeView = JASPWidgets.objectView.extend({

	menuName: "HtmlNode",

	 attachToolbar: function($toolbar) {
		 var title = this.model.get("title");

		 // if you change "hide me" here then also change it in Common.R and in zzzWrappers.R or come up with a way to define it in such a way to make it show EVERYWHERE...
		 if(title !== undefined && title !== "hide me")
			 this.$el.prepend(this.toolbar.$el);
	 },

	 copyMenuClicked: function () {
		 var exportParams = new JASPWidgets.Exporter.params();
		 exportParams.format = JASPWidgets.ExportProperties.format.raw;
		 exportParams.process = JASPWidgets.ExportProperties.process.copy;
		 exportParams.includeNotes = false;

		 pushTextToClipboard({raw: this.model.get("rawtext"), html: convertModelToHtml(this.model) } , exportParams)
		 return true;
	 },



	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
		'click': '_mouseClicked',
	},

	notePositionBottom: true,

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	_mouseClicked: function (e) {},

	hasCollapse: function () {
		return false;
	},

	constructChildren: function (constructor, data) {

		var self = this;
		this.toolbar.selectionElement = function () {
			return self.$el.find('th, td:not(.squash-left)');
		};

		var htmlPrimitive = new JASPWidgets.htmlNodePrimitive({ model: this.model, className: "jasp-html-primitive jasp-display-primitive" });
		this.localViews.push(htmlPrimitive);
		this.views.push(htmlPrimitive);
	},

	disableTitleExport: true,
});


JASPWidgets.htmlNodePrimitive = JASPWidgets.View.extend({

	render: function () {
		this.$el.append(convertModelToHtml(this.model));
	},

	getExportAttributes: function (element, exportParams) {
		var attrs = ""
		var style = ""

		var $elObj = $(element)
		var tag = $elObj.prop("tagName").toLowerCase()

		if (tag === "td" || tag === "th") {

			style = JASPWidgets.Exporter.getTableContentStyles($elObj, exportParams);

			if ($elObj.prop("rowspan") && $elObj.prop("rowspan") != 1)
				attrs += 'rowspan="' + $elObj.prop("rowspan") + '" '

			if ($elObj.prop("colspan") && $elObj.prop("colspan") != 1)
				attrs += 'colspan="' + $elObj.prop("colspan") + '" '
		}
		else if (tag === "table") {
			style = JASPWidgets.Exporter.getTableStyles($elObj, exportParams);
		}
		else if (tag === "span" || tag === "h1" || tag === "h2" || tag === "h3") {
			style = JASPWidgets.Exporter.getHeaderStyles($elObj, exportParams);
		}
		else if ($elObj.is('.error-message-positioner')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-positioner');
		}
		else if ($elObj.is('.error-message-box')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-box');
		}
		else if ($elObj.is('.error-message-symbol')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-symbol');
		}
		else if ($elObj.is('.error-message-message')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-message');
		}


		if (style)
			attrs = style + ' ' + attrs

		return attrs;
	},

	exportHTML: function (exportParams, element, tabs) {
		if (element == null)
			element = this.$el;

		tabs = tabs || ""

		var text = ""
		var $elObj = $(element)

		if ($elObj.hasClass("do-not-copy") || $elObj.is("td.squash-left"))
			return text

		var tag = $elObj.prop("tagName").toLowerCase()

		var attrs = this.getExportAttributes(element, exportParams);

		if (attrs)
			text = tabs + '<' + tag + ' ' + attrs + '>'
		else
			text = tabs + '<' + tag + '>'

		var contents = $elObj.contents()

		if (contents.length > 0) {

			for (var i = 0; i < contents.length; i++) {
				var node = contents[i]
				if (node.nodeType === 3) { //is text node
					var value = $(node).text()
					if (value) {

						value = value
							.replace(/&/g, '&amp;')
							.replace(/"/g, '&quot;')
							.replace(/'/g, '&#39;')
							.replace(/</g, '&lt;')
							.replace(/>/g, '&gt;')
							.replace(/\u2212/g, '-')

						text += "\n" + tabs + value + "\n"
					}
				}
				else {
					text += "\n" + this.exportHTML(exportParams, contents[i], tabs + "\t");
				}
			}

			text += tabs + '</' + tag + '>\n'
		}
		else {

			text += '</' + tag + '>\n'
		}

		return text;
	},

	exportBegin: function (exportParams, completedCallback) {

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		if (exportParams.includeNotes && this.noteBox !== undefined && this.noteBox.visible && this.noteBox.isTextboxEmpty() === false) {
			var exportObject = {
				views: [this, this.noteBox],
				getStyleAttr: function () {
					return "style='display: block;'";
				}
			};
			var newParams = exportParams.clone();
			newParams.includeNotes = false;

			JASPWidgets.Exporter.begin(exportObject, newParams, callback, true);
		}
		else
			callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, this.exportHTML(exportParams)));

		return true;
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},
});
