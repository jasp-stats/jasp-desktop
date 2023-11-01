var JASPWidgets = {};

var i18nStrsObj = {};
// The global translation function needs to be loaded before all other .js files.
window.setI18nStrings = function (i18nObject) {
		i18nStrsObj = i18nObject
		return i18nStrsObj
}

// Use i18n(...) to call strings to be translated then update them in MainPage.qml
function i18n(str) {
	if (typeof str === "string") {
		let findI18nStr = i18nStrsObj[str] !== null && i18nStrsObj[str] !== undefined;
		if (findI18nStr) {
			return i18nStrsObj[str];
		} else {
			return str;
		}
	} else {
		console.error("i18n can only be used for literal strings")
		return str;
	}
}

var videoUrlList = [];

window.sendUrlWhitelist = function (RequestedURL) {
	videoUrlList = RequestedURL; // to get from MainPage.qml
}

function isURLInWhitelist(hostname) {
	for (var i = 0; i < videoUrlList.length; i++) {
		var pattern = videoUrlList[i];
		if (pattern === hostname) {
			return true;
		} else if (pattern.indexOf('*') !== -1) {
			const regex = new RegExp(`^${pattern.replace(/\./g, '\\.').replace(/\*/g, '.*')}$`);
			if (regex.test(hostname)) {
				return true;
			}
		}
	}
	return false;
}

if (insideJASP) {
	var Parchment = Quill.import('parchment');

	var LineBreakClass = new Parchment.Attributor.Class('linebreak', 'linebreak', {
		scope: Parchment.Scope.BLOCK
	});

	Quill.register('modules/blotFormatter', QuillBlotFormatter.default);

	Quill.register('formats/linebreak', LineBreakClass);

	// See https://github.com/quilljs/quill/issues/262
	var Link = Quill.import('formats/link');
	Link.sanitize = function (url) {
		// Check if the url contains the protocol, otherwise add it automatically
		var checkUrl = url.match(/^(http|https):\/\//i);
		if (!checkUrl) {
			url = "https://" + url;
		}
		return url;
	}

	function customVideoUrl(url) {
		if (!/^(http|https):\/\//i.test(url)) {
			url = 'https://' + url;
		} else if (/^(http):\/\//i.test(url)) {
			url = url.replace(/^http:/i, 'https:');
		}
		let matchs = url.match(/^(?:(https?):\/\/)?(?:(?:www|m)\.)?youtube\.com\/watch.*v=([a-zA-Z0-9_-]+)/) ||
			url.match(/^(?:(https?):\/\/)?(?:(?:www|m)\.)?youtu\.be\/([a-zA-Z0-9_-]+)/) ||
			url.match(/^.*(youtu.be\/|v\/|e\/|u\/\w+\/|embed\/|v=)([^#\&\?]*).*/);
		if (matchs && matchs[2].length === 11) {
			return ('https') + '://www.youtube.com/embed/' + matchs[2] + '?showinfo=0';
		}
		if (matchs = url.match(/^(?:(https?):\/\/)?(?:www\.)?vimeo\.com\/(\d+)/)) {
			return (match[1] || 'https') + '://player.vimeo.com/video/' + matchs[2] + '/';
		}
		if (matchs = url.match(/(?:www\.|\/\/)bilibili\.com\/video\/(\w+)/)) {
			return 'https://player.bilibili.com/player.html?bvid=' + matchs[1]

		}
		if (matchs = url.match(/\/\/v\.qq\.com\/x\/cover\/.*\/([^\/]+)\.html\??.*/)) {
			return 'https://v.qq.com/txp/iframe/player.html?vid=' + matchs[1]
		}
		
		return url
	}

	const BlockEmbed = Quill.import("blots/block/embed");

	class EmbendVideo extends BlockEmbed {
		static create(value) {
			value = customVideoUrl(value)
			let node = super.create(value);
			let div = document.createElement('div');
				div.setAttribute("title", i18n("Unsupported video services"));
			$(div).append(`${i18n('JASP only allows the following videoservices:')}<br><br> <i>"Youtube, Vimeo, Bilibili, Tencent-Video"</i> <br><br>${i18n('Contact the JASP team to request adding another videoservice to the list.')}`)	
			node.setAttribute('frameborder', '0');
			node.setAttribute('allowfullscreen', true);
			node.setAttribute('src', value);
			if (!isURLInWhitelist((new URL(value).hostname)))
				node.innerHTML = $(div).dialog({ // give a warnning for unsupported urls and then remove from node.
					modal: true, buttons: {
						Ok: function () {
							$(node).remove();
							$(this).dialog("close");
						}
					}
				})
			return node;
		}
	}

	EmbendVideo.blotName = 'video';
	EmbendVideo.className = 'ql-video';
	EmbendVideo.tagName = 'IFRAME';

	Quill.register(EmbendVideo);

}

JASPWidgets.Encodings = {
	getBase64: function (arrayBuffer) {
		var base64 = ''
		var encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

		var bytes = new Uint8Array(arrayBuffer)
		var byteLength = bytes.byteLength
		var byteRemainder = byteLength % 3
		var mainLength = byteLength - byteRemainder

		var a, b, c, d
		var chunk

		// Main loop deals with bytes in chunks of 3
		for (var i = 0; i < mainLength; i = i + 3) {
			// Combine the three bytes into a single integer
			chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2]

			// Use bitmasks to extract 6-bit segments from the triplet
			a = (chunk & 16515072) >> 18 // 16515072 = (2^6 - 1) << 18
			b = (chunk & 258048) >> 12 // 258048   = (2^6 - 1) << 12
			c = (chunk & 4032) >> 6 // 4032     = (2^6 - 1) << 6
			d = chunk & 63               // 63       = 2^6 - 1

			// Convert the raw binary segments to the appropriate ASCII encoding
			base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d]
		}

		// Deal with the remaining bytes and padding
		if (byteRemainder == 1) {
			chunk = bytes[mainLength]

			a = (chunk & 252) >> 2 // 252 = (2^6 - 1) << 2

			// Set the 4 least significant bits to zero
			b = (chunk & 3) << 4 // 3   = 2^2 - 1

			base64 += encodings[a] + encodings[b] + '=='
		} else if (byteRemainder == 2) {
			chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1]

			a = (chunk & 64512) >> 10 // 64512 = (2^6 - 1) << 10
			b = (chunk & 1008) >> 4 // 1008  = (2^6 - 1) << 4

			// Set the 2 least significant bits to zero
			c = (chunk & 15) << 2 // 15    = 2^4 - 1

			base64 += encodings[a] + encodings[b] + encodings[c] + '='
		}

		return base64
	},


}

JASPWidgets.ExportProperties = {

	format: {
		raw: 0,
		html: 1, //bit 1
		formatted: 2, //bit 2
		formattedHTML: 3
	},

	process: {
		copy: 0,
		save: 1
	},

	htmlImageFormat: {
		temporary: 0,
		resource: 1,
		embedded: 2
	}
}

JASPWidgets.Exporter = {

	params: function () {

		this.format = JASPWidgets.ExportProperties.format.raw,
		this.process = JASPWidgets.ExportProperties.process.copy,
		this.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary,
		this.includeNotes = false;

		this.isFormatted = function () {
			return (this.format & JASPWidgets.ExportProperties.format.formatted) === JASPWidgets.ExportProperties.format.formatted
		};

		this.htmlOnly = function () {
			return (this.format & JASPWidgets.ExportProperties.format.html) === JASPWidgets.ExportProperties.format.html
		};

		this.clone = function () {
			var cloneParams = new JASPWidgets.Exporter.params();
			cloneParams.format = this.format;
			cloneParams.process = this.process;
			cloneParams.htmlImageFormat = this.htmlImageFormat;
			cloneParams.includeNotes = this.includeNotes;
			return cloneParams;
		};
	},

	data: function (raw, html) {

		if (raw == null)
			raw = "";

		this.raw = raw;
		this.html = html;
	},

	begin: function (exportObj, exportParams, completedCallback, useNBSP, innerStyle) {

		if (innerStyle === undefined)
			innerStyle = "";

		if (useNBSP == undefined)
			useNBSP = false;

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		if (exportObj.views) {
			var viewList = [];
			for (var i = 0; i < exportObj.views.length; i++) {
				var view = exportObj.views[i];
				if (exportParams.includeNotes || view.$el.hasClass('jasp-notes') === false) {
					if (view.$el.hasClass('removed') === false)
						viewList.push(view);
				}
			}

			if (viewList.length === 0)
				completedCallback.call(exportObj, exportParams, new JASPWidgets.Exporter.data(null, ""));
			else {
				exportObj.buffer = [];
				exportObj.exportCounter = viewList.length;

				for (var i = 0; i < viewList.length; i++)
					this._exportView(exportParams, viewList[i], i, exportObj, useNBSP, innerStyle, completedCallback);
			}
		}
		else if (exportObj.exportBegin)
			exportObj.exportBegin(exportParams, completedCallback);
		else
			return false;


		return true;
	},

	_exportView: function (exportParams, view, i, parent, useNBSP, innerStyle, completedCallback) {

		var self = parent;
		var index = i;
		var callback = completedCallback;

		var cc = function (exParams, exContent) {
			self.buffer[index] = exContent;
			self.exportCounter -= 1;
			if (self.exportCounter === 0) {
				var completeText = "";
				var raw = null;
				if (!exportParams.error) {
					completeText = `<div class="${self.className}" ${self.getStyleAttr()} >\n`;
					completeText += `<div style="display:inline-block; ${innerStyle}">\n`;
					if (!self.disableTitleExport && self.toolbar !== undefined) {
						completeText += JASPWidgets.Exporter.getTitleHtml(self.toolbar, exportParams)
					}
					var firstItem = true;
					for (var j = 0; j < self.buffer.length; j++) {
						if (self.buffer[j]) {
							if (self.buffer[j].raw !== null && self.buffer[j].raw !== '')
								raw = self.buffer[j].raw;

							var bufferHtml = self.buffer[j].html;
							if (bufferHtml !== '') {
								var includeSpacer = false;
								if (exParams.format !== JASPWidgets.ExportProperties.format.formattedHTML) {
									if (firstItem === false && (JASPWidgets.Exporter.isInlineStyle(self.views[j - 1].$el) == false)) {
										if ((this.hasExportNSBFOverride && this.hasExportNSBFOverride()) || this.useExportNSBF)
											includeSpacer = this.useExportNSBF();
										else
											includeSpacer = useNBSP;
									}
								}

								if (includeSpacer)
									completeText += "&nbsp;";

								completeText += self.buffer[j].html;
								firstItem = false;
							}
						}
					}
					completeText += "</div>";
					completeText += "</div>";
				}

				if (parent.exportWrapper)
					completeText = parent.exportWrapper(completeText);

				callback.call(self, exportParams, new JASPWidgets.Exporter.data(raw, completeText));
				self.buffer = [];
			}
		}
		view.exportBegin(exportParams, cc);
	},

	getStyles: function (element, styleItems) {

		var css = element.css(styleItems);
		if (css === undefined)
			return "";

		var style = "";
		for (var i = 0; i < styleItems.length; i++)
		{
			var styleItem = styleItems[i];
			if (css[styleItem])
				style += styleItem + ": " + css[styleItem] + "; "
		}

		if (style)
			style = "style='" + style + "'";

		return style;
	},

	getSpacingStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["padding", "margin", "display", "float"]);
		else
			return "";//JASPWidgets.Exporter.getStyles(element, ["display", "float"]);
	},

	getHeaderStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["padding", "text-align", "margin", "display", "float", "vertical-align", "font-size", "font", "font-weight", "color"]);
		else
			return "";//JASPWidgets.Exporter.getStyles(element, ["display", "float"]);
	},

	getTableStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align", "margin-bottom", "margin-top", "display", "float", "color"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "display", "float"]);
	},

	getTableContentStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align", "margin", "display", "float", "font-size", "font-weight", "font", "color"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "display", "float", "text-align"]);
	},

	getErrorStyles: function (element, component) {
		if (component === "error-message-positioner")
			return JASPWidgets.Exporter.getStyles(element, ["padding", "margin", "display", "float", "color", "height", "overflow", "position", "top", "z-index"]);
		else if (component === "error-message-box")
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "display", "float", "border-radius", "min-width", "max-width", "white-space"]);
		else if (component === "error-message-symbol ")
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "display", "float"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["margin", "border", "background-color", "color", "padding", "text-align", "display", "float", "vertical-align", "font-size", "font", "font-weight"]);
	},

	getNoteStyles: function (element, exportParams) {
		if (exportParams.isFormatted())
			return JASPWidgets.Exporter.getStyles(element, ["margin", "padding", "max-width", "min-width", "display", "font-size", "font-weight", "font", "color", "border-top-style", "border-top-width", "border-top-color", "border-bottom-style", "border-bottom-width", "border-bottom-color"]);
		else
			return JASPWidgets.Exporter.getStyles(element, ["max-width", "min-width"]);
	},

	exportErrorWindow: function (element, error) {
		var text = "";

		if (error && error.errorMessage) {

			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element, 'error-message-positioner') + '>'
			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-box'), 'error-message-box') + '>'
			text += '<span ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-symbol'), 'error-message-symbol') + '></span>'
			text += '<div ' + JASPWidgets.Exporter.getErrorStyles(element.find('.error-message-message'), 'error-message-message') + '>' + error.errorMessage + '</div>'
			text += '</div>'
			text += '</div>'
		}

		return text;
	},

	isInlineStyle: function (element) {
		var css = element.css("display");

		return css === "inline" || css === "inline-block";
	},

	getTitleHtml: function (toolbar, exportParams) {
		var html = toolbar.title === undefined ? "" : toolbar.title;
		if (toolbar.titleTag !== undefined) {
			var headerStyles = " " + JASPWidgets.Exporter.getHeaderStyles(toolbar.$title(), exportParams);
			html = '<' + toolbar.titleTag + headerStyles + '>' + toolbar.title + '</' + toolbar.titleTag + '>';
		}

		var topLevelStyles = " " + JASPWidgets.Exporter.getSpacingStyles(toolbar.$el, exportParams);
		return '<div ' + topLevelStyles + '>' + html + '</div>';
	},
};

JASPWidgets.View = Backbone.View.extend({

	inCollection: false,

	getStyleAttr: function (styleItems) {

		return JASPWidgets.Exporter.getStyles(this.$el, ["padding", "text-align", "margin-bottom", "margin-top", "margin-left", "margin-right", "display", "float", "position"]);

	},

	_isClosed: false,
	/** Removes DOM tree elements and any listeners from or too the view object */
	close: function () {
		if (!this._isClosed) {
			if (this.onClose)
				this.onClose();

			this.remove();
			this.off();
			this._isClosed = true;
		}
	},
})

JASPWidgets.Note = Backbone.Model.extend({
	defaults: {
		text: '<p><br></p>',
		format: 'markdown',
		delta: {},
		deltaAvailable: false,
	},

	toHtml: function () {
		if (this.get('format') === 'markdown') {
			this.set('format', 'html');
			var text = this.get('text');
			if (text === null || text === '')
				this.set('text', '<p><br></p>');
			else if (text !== '')
				this.set('text', Mrkdwn.toHtml(text));
		}
	},
});

JASPWidgets.NoteBox = JASPWidgets.View.extend({

	//#7C95CB
	//#F2F7FD

	initialize: function () {

		this.ghostTextDefault = i18n('Click here to add text');

		this.editing = false;

		this.visible = this.model.get('visible');
		if (this.visible === undefined || this.visible === null)
			this.visible = false;

		this.internalChange = false;

		if (this.model.get('format') === 'markdown')
			this.model.toHtml();

		// this.listenTo(this.model, 'change:text', this.textChanged)

		this.closeButton = new JASPWidgets.ActionView({ className: "jasp-closer" });
		var self = this;
		this.closeButton.actionTargetElement = function () {
			return self.$el;
		};
		this.closeButton.setAction(function () {

			self.setVisibilityAnimate(false);
			if (window.resultsDocumentChanged)
				window.resultsDocumentChanged();
		});
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
		'mousedown' : '_handleMouseDown',
	},

	detach: function() {
		this.$el.detach();
		this.closeButton.$el.detach();
	},

	_hoveringStart: function (e) {
		this.closeButton.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.closeButton.setVisibility(false);
	},

	_handleMouseDown: function (e) {
		this.setQuillToolbarVisibility('block');
	},

	clear: function () {

		this.model.set('format', 'html');
		this.model.set('text', '');
		this.model.set('delta', {});
		this.model.set('deltaAvailable', false);
	},

	isTextboxEmpty: function () {

		//We should probably only be here if we have $quill right?
		if(this.$quill === undefined)
			return undefined;

		return this.$quill.getLength() === 0;
	},

	render: function () {

		if(!insideJASP)	return; //We dont want noteboxes in a dashboard

		if (this._inited) {
			this.$quill.off();
			delete this.$quill;
		}

		this.$el.empty();

		this.setVisibility(this.visible)

		var html = this.model.get("text");

		this.closeButton.render();

		this.$el.append('<div class="jasp-hide" data-button-class="jasp-comment"></div>');
		this.$el.append("<div id=\"editor\"></div>");

		var toolbarOptions = [
			['bold', 'italic', 'underline', 'link'], ['formula', 'code-block', 'image', 'video'],
			// [{ 'size': ['small', false, 'large', 'huge'] }],
			[{ 'header': [1, 2, 3, 4, false] }, { 'list': 'ordered'}, { 'list': 'bullet' }],
			[{ 'color': [] }, { 'background': [] }],
			[{ 'script': 'sub'}, { 'script': 'super' }],
			['blockquote', { 'indent': '+1'}, { 'indent': '-1' }],
			// [{ 'font': [] }, { 'align': [] }],
			[{ 'size': [ 'small', false, 'large' ]}],
			['clean']
		];

		let placeholderText = this.ghostTextDefault
		if (typeof this.ghostText !== 'undefined')
			placeholderText = this.ghostText;

		// here configure code highlight global
		hljs.configure({
			languages: ['r'] // optionally highlight language(s)
		});

		var options = {
			theme: 'snow',
			modules: {
				syntax: true,
				toolbar: toolbarOptions,
				blotFormatter: {
					specs: [QuillBlotFormatter.ImageSpec]
				  },
				keyboard: {
					bindings: {
						smartbreak: {
							// Handle shift-Enter. Cf https://github.com/quilljs/quill/issues/252
							key: 13,
							shiftKey: true,
							handler: function (range, context) {
								this.quill.setSelection(range.index,'silent');
								this.quill.insertText(range.index, '\n', 'user')
								this.quill.setSelection(range.index +1,'silent');
								this.quill.format('linebreak', true,'user');
							}
						},
						paragraph: {
							key: 13,
							handler: function (range, context) {
								this.quill.setSelection(range.index,'silent');
								this.quill.insertText(range.index, '\n', 'user')
								this.quill.setSelection(range.index +1,'silent');
								let f = this.quill.getFormat(range.index +1);
								if(f.hasOwnProperty('linebreak')) {
									delete(f.linebreak)
									this.quill.removeFormat(range.index +1)
									for(let key in f){
										this.quill.formatText(range.index +1,key,f[key])
									}
								}
							}
						}
					}
				}
			},
			placeholder: placeholderText
		};

		let targetDiv = this.$el.find("#editor").get(0);
		this.$quill = new Quill(targetDiv, options)

		var self = this;
		var delt;

		this.$quillToolbar     = this.$el.find(".ql-toolbar").get(0);
		let quillEditorElement = this.$el.find(".ql-editor").get(0);
		
		this.$quillTooltip     = this.$el.find(".ql-tooltip");
		var quillThemeTooltip  = this.$quill.theme.tooltip;

		// Change example link from quilljs.com to a sample link
		var linkInput = quillThemeTooltip.root.querySelector('input[data-link]');
			linkInput.dataset.link = 'https://jasp-stats.org';

		// Add tooltips to the toolbar buttons
		// Quilljs website mentions changing the toolbar html element (https://quilljs.com/playground/#snow-toolbar-tooltips),
		//     however, that is not handy for complex buttons such as color picker
		//     Instead, we will use the browser standard "title" attribute as
		//     mentioned here: https://github.com/quilljs/quill/issues/1271#issuecomment-597928093

		this.$quillToolbar.querySelector('button.ql-bold').setAttribute('title', i18n('Bold'));
		this.$quillToolbar.querySelector('button.ql-italic').setAttribute('title', i18n('Italic'));
		this.$quillToolbar.querySelector('button.ql-underline').setAttribute('title', i18n('Underline'));
		this.$quillToolbar.querySelector('button.ql-link').setAttribute('title', i18n('Link'));

		this.$quillToolbar.querySelector('button.ql-formula').setAttribute('title', i18n('Formula'));
		this.$quillToolbar.querySelector('button.ql-code-block').setAttribute('title', i18n('Code Block'));
		this.$quillToolbar.querySelector('button.ql-image').setAttribute('title', i18n('Image'));
		this.$quillToolbar.querySelector('button.ql-video').setAttribute('title', i18n('Embed web video'));

		this.$quillToolbar.querySelector('.ql-header.ql-picker').setAttribute('title', i18n('Header'));
		let lists = this.$quillToolbar.querySelectorAll('button.ql-list')
		lists[0].setAttribute('title', i18n('Ordered List'))
		lists[1].setAttribute('title', i18n('Unordered List'))

		this.$quillToolbar.querySelector('.ql-color.ql-picker.ql-color-picker').setAttribute('title', i18n('Color Picker'));
		this.$quillToolbar.querySelector('.ql-background.ql-picker.ql-color-picker').setAttribute('title', i18n('Background Color'));

		let scripts = this.$quillToolbar.querySelectorAll('button.ql-script')
		scripts[0].setAttribute('title', i18n('Subscript'))
		scripts[1].setAttribute('title', i18n('Superscript'))

		this.$quillToolbar.querySelector('button.ql-blockquote').setAttribute('title', i18n('Blockquote'));
		let indents = this.$quillToolbar.querySelectorAll('button.ql-indent')
		indents[0].setAttribute('title', i18n('Add Indent'))
		indents[1].setAttribute('title', i18n('Remove Indent'))

		this.$quillToolbar.querySelector('.ql-size.ql-picker').setAttribute('title', i18n('Font Size'));
		this.$quillToolbar.querySelector('button.ql-clean').setAttribute('title', i18n('Clear Formatting'));

		// Custom mouse events for the toolbar
		this.$quillToolbar.addEventListener('mousedown', (event) => {
			event.preventDefault();
		});

		quillEditorElement.addEventListener('click', (event) => {
			this.setQuillToolbarVisibility('block') //set toobar visiable;

			//// LaTex editor
			let $formulaNode = this.$el.find('.ql-editor mjx-container')
			 $formulaNode.on('click', (e) => {
				let currentFormula = e.currentTarget
				let formulaBlot = Quill.find(currentFormula);

				let index = formulaBlot.offset(this.$quill.scroll);
				let line = this.$quill.getIndex(formulaBlot)

				this.oldBlot = formulaBlot // Get legacy formula range to remove while save

				quillThemeTooltip.edit('formula', currentFormula.getAttribute('data-value'));

				let saveFunction = quillThemeTooltip.save;
				quillThemeTooltip.save = () => {
 					if (this.oldBlot)
						this.oldBlot.remove();
					saveFunction.call(quillThemeTooltip);
					this.oldBlot = null;
				};
			});

			//// Image resizer
			let $imgBlot = this.$el.find('.ql-editor p img');
            let $blotResizer = this.$el.find('.blot-formatter__overlay');
			let $resizeHandles = this.$el.find('[class^="blot-formatter"]');

			// auto show/hide resizer handles while hover/leave.
            $blotResizer.on( "mouseenter", ()=> { $resizeHandles.show()} ).on( "mouseleave", ()=> { $resizeHandles.hide()} );
            $imgBlot.on(     "mouseenter", ()=> { $resizeHandles.show()} ).on( "mouseleave", ()=> { $resizeHandles.hide()} );

		});

		quillEditorElement.addEventListener('focusout', (event) => {
		    // Always keep editor available while a tooltip editor show
		    if (this.$quillTooltip.is(':visible')) {
				return;
		    } else {
				self.setQuillToolbarVisibility('none');
		    }
		});

		if (this.model.get('deltaAvailable')) {
			delt = this.model.get('delta');
		} else {
			if (this.model.get('format') === 'markdown') {
				this.model.toHtml();
				html = this.model.get("text");
			}

			delt = this.$quill.clipboard.convert(html);
		}

		this.$quill.setContents(delt);
		self.onNoteChanged(self.$quill.root.innerHTML, self.$quill.getContents());

		this.$quill.on('text-change', function(delta, oldDelta, source) {
			self.onNoteChanged(self.$quill.root.innerHTML, self.$quill.getContents());
		});

		this.setQuillToolbarVisibility('none');

		this._inited = true;

		return this;
	},

	onNoteChanged: function (html, quDelta) {

		this.internalChange = true;

		this.model.set({
			'text': html,
			'format': 'html',
			'delta': quDelta,
			'deltaAvailable' : true
		});

		this.internalChange = false;

		if (this._textedChanging === true)
			return;

		this._textedChanging = true;
		if (this._inited)
			this.trigger("NoteBox:textChanged");
		this._textedChanging = false;
	},

	setQuillToolbarVisibility: function(display) {
		// display: ['block', 'none']

		if(!insideJASP) return;

		this.$quillToolbar.style.display = display;

		if (display === 'block') {
			this.$el.removeClass('jasp-notes-border')
		} else {
			this.$el.addClass('jasp-notes-border')
		}
	},

	setVisibility: function(value) {
		this.visible = value;

		if (value)
			this.$el.removeClass('jasp-hide');
		else
			this.$el.addClass('jasp-hide');
	},

	setVisibilityAnimate: function (value, scroll) {

		var self = this;
		var scrollIntoView = scroll === undefined ? true : scroll;
		self.$el.css("opacity", value ? 0 : 1);

		if (value === true) {
			self.$el.slideDown(200, function () {
				self.setVisibility(value);
				self.$el.animate({ "opacity": 1 }, 200, "easeOutCubic", function () {
					if (scrollIntoView) {
						window.scrollIntoView(self.$el, function () {});
					}
				});
			});
		}
		else {
			self.$el.slideUp(200, function () {
				self.setVisibility(value);
			});
		}
	},

	_keydown: function (e) {
		var self = e.data;
		if (e.which == 9) {
			e.preventDefault();
		}
		else if (e.which === 13 && e.ctrlKey) {
			e.preventDefault();
		}
		else if (e.which === 27) {
			e.preventDefault();
		}
		else if (e.which === 66 && e.ctrlKey) { //ctrl+b
			document.execCommand('bold', false, null);
		}
		else if (e.which === 73 && e.ctrlKey) { //ctrl+i
			document.execCommand('italic', false, null);
		}
		else if (e.which === 187 && e.ctrlKey) { //ctrl+=
			if (e.shiftKey)
				document.execCommand('superscript', false, null); //ctrl+shift+=
			else
				document.execCommand('subscript', false, null); //ctrl+=
		}
	},

	_mousedown: function (e) {
		var self = e.data;

		self._setEdittable(e.pageX, e.pageY);

		return true;
	},

	_setEdittable: function (pageX, pageY) {

		if (this.editing === true)
			return;

		this.editingSetup = true;

		etch.config.selector = '.jasp-editable'

		_.extend(etch.config.buttonClasses, {
			'default': ['bold', 'italic', 'underline'],
			'jasp-comment': ['bold', 'italic', 'superscript', 'subscript', 'unordered-list', 'ordered-list']
		});

		this.editing = true;
		var self = this;

		window.setTimeout(function () { self.editingSetup = false; }, 0); //needsd to wait for all ui events to finish before ending
	},

	exportBegin: function (exportParams, completedCallback) {

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		var html = '';
		if (insideJASP && this.isTextboxEmpty() === false && this.visible === true) {

			html += '<div ' + JASPWidgets.Exporter.getNoteStyles(this.$el, exportParams) + '>' + this.$quill.root.innerHTML + '</div>';
		}

		callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, html));
	},

	exportComplete: function (exportParams, exportContent) {

		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},

	useExportNSBF: function() {
		return false;
	},
})


JASPWidgets.Toolbar = JASPWidgets.View.extend({
	initialize: function () {
		$(document).on("mousedown", this, this._mouseDownGeneral);
		this.fixed = false;
		this.visible = false;
		this.selected = false;
		this.editing = false;
	},

	$title: function() {
		return this.$el.find(this.titleTag);
	},

	setStatus: function(status) {
		this.status = status;
	},

	render: function () {
		this.$el.empty();

		if (this.titleTag !== undefined)
			this.$el.addClass(this.titleTag + "-toolbar");

		if (this.title !== undefined) {
			if (this.titleTag !== undefined)
				this.$el.append('<' + this.titleTag + ' class="in-toolbar toolbar-clickable">' + this.title + '</' + this.titleTag + '>');
			else
				this.$el.append(this.title);
		}

		if (this.hasMenu)
		{
			this.$el.append('<div class="toolbar-button toolbar-clickable jasp-menu jasp-hide"/>')

			//var $menuBtn = this.$el.find(".jasp-menu")

			var $self = this.$el.find(">:first-child");
			$self.tooltip({
				content: "...",
				items: "*",
				disabled: true,
				show: {
					duration: 250,
					effect: "fade",
					delay: 250
				},
				close: function () {
					window.setTimeout(function () {
						$self.tooltip("option", "disabled", true)
					}, 2000)
				},
				position: {
					my: "left top",
					at: "left bottom+5",
					collision: "flipfit"
				}
			})
		}

		if (this.status !== undefined)
			this.$el.append('<div class="status ' + this.status + '"></div')

		return this;
	},

	events: {
		'mousedown .toolbar-clickable': '_mouseDown',
		'focusout': '_looseFocus',
		'keydown .in-toolbar': '_keydown',

	},

	startEdit: function (callbackWhenDone) {
		this.editing	= true;
		var element		= this.$title();

		this["callback"] = callbackWhenDone;

		element.addClass("toolbar-editing");
		element[0].setAttribute("contenteditable", true);

		element.focus();

		element.on("focus", "paste", function (event) {
			var pastedData = event.originalEvent.clipboardData.getData('text/plain');
			this.innerHTML = pastedData;
			event.preventDefault();
		});
	},

	endEdit: function (saveTitle) {
		if (this._editEnding)
			return;

		this._editEnding	= true;
		var element			= this.$title();
		element.removeClass("toolbar-editing");

		element[0].setAttribute("contenteditable", false);
		this.editing	= false;
		var selection	= window.getSelection();
		selection.removeAllRanges();

		element.off("paste");

		if (saveTitle)	this.setTitle(element.text());
		else			element.html(this.title);

		this._editEnding = false;

		if(this["callback"] !== undefined && this["callback"] !== null)
		{
			this["callback"](this.title);
			this["callback"] = null;
		}
	},

	setTitle: function(title) {
		this.title = title;
	},

	_looseFocus: function () {
		this.endEdit(true);
	},

	_keydown: function (e) {
		if (!this.editing)
			return;

		if (e.which == 9) {
			e.preventDefault();
		}
		else if (e.which == 13) {
			e.preventDefault();
			this.endEdit(true);
		}
		else if (e.which == 27) {
			e.preventDefault();
			this.endEdit(false);
		}
	},

	_mouseDownGeneral: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		self.decreaseFixedness();
	},

	setVisibility: function (value) {
		this.visible = value;

		if (this.fixed)
			return;

		if (value) {
			this.$(".toolbar-button").removeClass('jasp-hide');
		}
		else {
			this.$(".toolbar-button").addClass('jasp-hide');
		}
	},

	_mouseDown: function (e) {

		if (this.editing)
			return true;

		if (this.parent.isCollapsed && this.parent.isCollapsed() && this.parent.setCollapsedState) {
			this.parent.setCollapsedState(false);
			return true;
		}

		this.setFixedness(2);

		var $titleLabel = this.$el.find('>:first-child');

		var offset = $titleLabel.offset();

		var posY = offset.top + $titleLabel.height() - $(window).scrollTop() + 3;
		var posX = offset.left - $(window).scrollLeft();
		var posXright = offset.left + $titleLabel.outerWidth() - $(window).scrollLeft();

		this.options.rX = posX;
		this.options.rY = posY;
		this.options.rXright = posXright;

		this.options.x = e.screenX;
		this.options.y = e.screenY;

		if (this.options.hasNotes)
			this.options['noteOptions'] = this.parent.noteOptions();

		if (this.options.hasCollapse)
			this.options['collapseOptions'] = this.parent.collapseOptions();

		this.parent.trigger('toolbar:showMenu', this.parent, this.options);

		return true;
	},

	decreaseFixedness: function() {
		this.setFixedness(this.fixed - 1);
	},

	setFixedness: function (value) {
		this.fixed = Math.max(0, value);
		this.setVisibility(this.visible);
		this.setSelected(this.fixed !== 0);
	},

	setParent: function (parent) {
		this.parent = parent;
		this.options = {
			//If you add something here don't forget to do the same in resultmenumodel.cpp
			hasCopy:				(parent.hasCopy			=== undefined || parent.hasCopy())			&& parent.copyMenuClicked			!== undefined,
			hasCite:				(parent.hasCitation		=== undefined || parent.hasCitation())		&& parent.citeMenuClicked			!== undefined,
			hasNotes:				(parent.hasNotes		=== undefined || parent.hasNotes())			&& parent.notesMenuClicked			!== undefined,
			hasSaveImg:				(parent.isConvertible	=== undefined || parent.isConvertible())	&& parent.saveImageClicked			!== undefined,
			hasEditImg:				(parent.isEditable		=== undefined || parent.isEditable())		&& parent.editImageClicked			!== undefined,
			hasEditTitle:			(parent.hasEditTitle	=== undefined || parent.hasEditTitle())		&& parent.editTitleClicked			!== undefined,
			hasRemove:				(parent.hasRemove		=== undefined || parent.hasRemove())		&& parent.removeMenuClicked			!== undefined,
			hasDuplicate:			(parent.hasDuplicate	=== undefined || parent.hasDuplicate())		&& parent.duplicateMenuClicked		!== undefined,
			hasShowDeps:			(parent.hasShowDeps		=== undefined || parent.hasShowDeps())		&& parent.showDependenciesClicked	!== undefined,
			hasCollapse:			(parent.hasCollapse		=== undefined || parent.hasCollapse())		&& parent.collapseMenuClicked		!== undefined,
			hasLaTeXCode:			(parent.hasLaTeXCode	=== undefined || parent.hasLaTeXCode())		&& parent.latexCodeMenuClicked		!== undefined,
			hasRemoveAllAnalyses:	parent.menuName			=== 'All',
			hasRefreshAllAnalyses:	parent.menuName			=== 'All',
			hasExportResults:		parent.menuName			=== 'All',
			hasShowRSyntax:			parent.menuName			=== 'All',

			objectName:				parent.menuName
		};

		this.hasMenu =	this.options.hasCopy			|| this.options.hasCite		|| this.options.hasSaveImg		|| this.options.hasEditImg		||
						this.options.hasDuplicate		|| this.options.hasNotes	|| this.options.hasRemove		|| this.options.hasRemoveAll	||
						this.options.hasEditTitle		|| this.options.hasCollapse || this.options.hasLaTeXCode	|| this.options.hasShowDeps		||
						this.options.hasExportResults	 ;
	},

	selectionElement: function() {	return this.parent.$el;	},

	setSelected: function (value) {
		this.selected			= value;
		var $selectionElement	= this.selectionElement();

		if (value)	$selectionElement.addClass(		"jasp-menu-selected")
		else		$selectionElement.removeClass(	"jasp-menu-selected")

	},

	displayMessage: function (msg) {
		if (msg !== undefined) {
			var $self = this.$el.find(">:first-child");
			$self.tooltip("option", "content", msg)
			$self.tooltip("option", "disabled", false)
			$self.tooltip("open")

			window.setTimeout(function () {	$self.tooltip("close")	}, 2000)
		}
	},

	completeEvent: function () {
		this.setFixedness(0);
	}
})

JASPWidgets.RSyntaxModel = Backbone.Model.extend({
	defaults: {
		analysis: {},
		script: ""
	},
	getFromAnalysis: function(item) {
		return this.attributes.analysis.model.get(item);
	}

});

JASPWidgets.RSyntaxView = JASPWidgets.View.extend({
	initialize: function() {
		this.$el.addClass("jasp-rsyntax-container");
		this.$el.addClass("jasp-hide");
		this.$el.addClass("jasp-code");
		this._insertRSyntax()
	},
	setVisibility: function (value) {
		var self = this;
		self.$el.css("opacity", value ? 0 : 1);

		if (value === true) {
			self.$el.slideDown(200, function () {
				self._setVisibility(value);
				self.$el.animate({ "opacity": 1 }, 200, "easeOutCubic", function () {});
			});
		}
		else {
			self.$el.slideUp(200, function () {
				self._setVisibility(value);
			});
		}
	},

	_setVisibility: function(value) {
		this.visible = value
		if (value)
			this.$el.removeClass('jasp-hide');
		else
			this.$el.addClass('jasp-hide');
	},
	render: function() {
		let rScript = this.model.get("script").replace(/<br>/g, "\n");
		this.$el.find(".jasp-rsyntax")
					.html("<pre><code class='language-r'>" + rScript + "</code></pre>");
		setTimeout(() => {
			this.$el.find(".jasp-rsyntax")[0].querySelectorAll('pre code').forEach((el) => {
				el.innerHTML = el.textContent; //Suppress warnings of html code tags
				hljs.highlightElement(el);
			});
		}, 200);
	},
	_insertRSyntax: function() {
		$script = $("<span/>");
		$script.attr({
		  class: "jasp-rsyntax",
		  id: "rsyntax-" + this.model.getFromAnalysis("id")
		});

		this.$el.append($script);
	},
	setScript: function(value) {
		this.model.set("script", value);
		this.render();
	},
	clear: function() {
		this.$el.empty();
		this.initialize();
	},

	exportBegin: function (exportParams, completedCallback) {
		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		var html = '';
		if (this.visible === true) {
			html += '<div ' + JASPWidgets.Exporter.getStyles(this.$el, ["padding", "border", "background-color", "font-size", "font", "font-weight", "display"]) + '>' + this.$el.get(0).innerHTML + '</div>';
		}

		callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, html));
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	}

});

JASPWidgets.Progressbar = Backbone.Model.extend({
	defaults: {
		analysis: {},
		label: "",
		value: -1,
		maxValue: 100
	},

	getFromAnalysis: function(item) {
		return this.attributes.analysis.model.get(item);
	}
});

JASPWidgets.ProgressbarView = JASPWidgets.View.extend({
	initialize: function() {
		this.$el.addClass("jasp-progressbar-container");
		this.fadeOutActive = false;
	},

	render: function() {
		if(this.model.getFromAnalysis("progress") === null)
		{
			this._resetModel();
			this._fadeOut();
		}
		else if(this.model.getFromAnalysis("progress") !== undefined)
		{
			var label = this.model.getFromAnalysis("progress").label;
			var value = this.model.getFromAnalysis("progress").value;

			if(label !== undefined && value !== undefined)
			{

				if (this._blockRequest(value)) {
					return this;
				} else if (this._needsToComplete(value)) {
					label = this.model.get("label");
					value = this.model.get("maxValue");
				} else {
					label = this._makePrettyLabel(label);
					value = Math.min(this.model.get("maxValue"), value)
				}

				this.model.set("value", value);
				this.model.set("label", label);
			}

			this.clear();
			this._insertBar();

			if (this.isComplete()) {
				this._resetModel();
				this._fadeOut();
			}
		}

		return this;
	},

	clear: function() {
		this.$el.empty();
		this.initialize();
	},

	isActive: function() {
		return -1 < this.model.get("value") && this.model.get("value") < this.model.get("maxValue");
	},

	isComplete: function() {
		return this.model.get("maxValue") <= this.model.get("value");
	},

	_resetModel: function() {
		var defaults = this.model.defaults;
		defaults.analysis = this.model.get("analysis");
		this.model.clear().set(defaults);
	},

	_fadeOut: function() {
		this.fadeOutActive = true;
		var self = this;
		window.setTimeout(function() {
			if (self.fadeOutActive) { // no new progressbar was made in the mean time
				self._getCurrent().fadeOut(750, function() {
					self.clear();
				});
			}
		}, 250);
	},

	_getCurrent: function() {
		return this.$el.find(".jasp-progressbar");
	},

	_insertBar: function() {
		$container = $("<div/>");
		$container.attr({
			class: "jasp-progressbar",
			id: "progressbar-" + this.model.getFromAnalysis("id")
		});

		$progressbar = $("<progress class=''></progress>");
		$progressbar.attr({
			value: this.model.get("value"),
			max: this.model.get("maxValue")
		});

		$label = $("<span/>");
		$label.attr({
			class: "jasp-progressbar-label"
		});
		$label.html(this.model.get("label"));

		$container.append($progressbar);
		$container.append($label);

		this.$el.append($container);
	},

	_makePrettyLabel: function(label) {
		return this._addTrailingEllipsis(this._truncate(label));
	},

	_truncate: function(label) {
		var maxChars = 80;
		var sep = "...";
		if (maxChars < label.length) {
			var nCharsPerChunk = Math.floor((maxChars - sep.length) / 2);
			label = label.substring(0, nCharsPerChunk) + '<span class="jasp-progressbar-label-sep">' + sep + '</span>' + label.substring(label.length - nCharsPerChunk);
		}
		return label;
	},

	_addTrailingEllipsis: function(label) {
		if (label.length == 0)
			return label;

		var alphaNumericOrDotEnding = label.match(/[a-z0-9.]$/i)||[];
		if (alphaNumericOrDotEnding.length == 0)
			return label; // might look weird otherwise, e.g., ~~~...

		var endingDots = label.match(/\.+$/g)||[""];
		var numDotsToAdd = 3 - endingDots[0].length;
		if (numDotsToAdd < 0)
			return label.slice(0, numDotsToAdd);
		return label + ".".repeat(numDotsToAdd);
	},

	_blockRequest: function(value) {
		return !this.isActive() && (value == -1 || value >= this.model.get("maxValue"));
	},

	_needsToComplete: function(value) {
		return this.isActive() && (value == -1 || this.model.getFromAnalysis("status") == "complete");
	}
});

JASPWidgets.ActionView = JASPWidgets.View.extend({
	initialize: function () {
		this.$el.addClass('jasp-hide');
	},

	actionDisabled: function () {
		return false;
	},

	actionTargetElement: function () {
		return null;
	},

	setAction: function(action) {
		this.actionCallback = action;
	},

	render: function () {
		this.actionTargetElement().append(this.$el)

		return this;
	},

	events: {
		'click': '_clickHandler'
	},

	_clickHandler: function (e) {
		if (!this.actionDisabled())
			this.actionCallback.call(this);
	},

	setVisibility: function (value) {
		if (value && !this.actionDisabled())
			this.$el.removeClass('jasp-hide');
		else
			this.$el.addClass('jasp-hide');
	}
});

JASPWidgets.Resizeable = Backbone.Model.extend({

	defaults: { width: 150, height: 150 },

	setDim: function (w, h) {
		this.set({ width: w, height: h });
	}
});


JASPWidgets.ResizeableView = JASPWidgets.View.extend({

	/** Property used to silence events from firing when needed. This is not to be set directly. Set using 'resizeStart' method.*/
	silent: false,

	/** Initialises the class by assigning listeners to the model for external size changes and for global mouse events. */
	initialize: function () {
		this._resizeableViewBase = JASPWidgets.View.prototype;

		this.$el.addClass('jasp-hide');

		$(document).on("mousemove", this, this._mousemove).on("mouseup", this, this._mouseup);
		this.listenTo(this.model, 'change:width', this.onModelChange);
		this.listenTo(this.model, 'change:height', this.onModelChange);
	},

	/**
	* Returns the DOM tree element whose size attributes with be affected during resizing.
	This allows for the inheriting class to choose another element when required. See image.js
	* @return {Object} DOM Tree element
	*/
	resizeTargetElement: function() {
		return null;
	},

	/**
	* Renders the mouse resize button to the targeted DOM tree element.
	* @return {Object} 'this' for chaining.
	*/
	render: function () {
		this.resizeTargetElement().append(this.$el)

		this._refreshView();
		return this;
	},

	/** Called when an external event changes the views model obejct. */
	onModelChange: function () {
		var size = this._refreshView();
		this.onResized(size.width, size.height);
	},

	/**
	* Called when the model has been resized.
	* @param {Number} w - The width the model was resied to.
	* @param {Number} h - The height the model was resied to.
	*/
	onResized: function (w, h) {
		this.trigger("ResizeableView:resized", w, h)
	},

	/**
	* Refreshes the size of the view using the model information.
	* @return {Object} The size of the view. The object has a width and height property.
	*/
	_refreshView: function(){
		var w = this.model.get('width');
		var h = this.model.get('height');
		this.resizeView(w, h);
		return {
			width: w,
			height: h
		}
	},

	/**
	* Defines the maximum and minimum sizes the view can be resied to.
	* @return {Object} The object has a minWidth, minHeight, maxWidth, maxHeight properties.
	*/
	sizeLimit: function () {
		return {
			minWidth: 160,
			minHeight: 160,
			maxWidth: 2000,
			maxHeight: 2000
		};
	},

	/**
	* Defines the maximum and minimum sizes the view can be resied to.
	* @return {Object} The object has a minWidth, minHeight, maxWidth, maxHeight properties.
	*/
	_mouseResizingStart: function (e) {

		this.iX = e.pageX;
		this.iY = e.pageY;
		var limited = this._normaliseSize(this.model.get("width"), this.model.get("height"));
		this.iW = limited.width;
		this.iH = limited.height;
		this.mouseResizing = true;
		this.resizeStart(this.iW, this.iH, false);
		return false;
	},

	_normaliseSize: function(w, h) {
		var normalised = {};
		var limit = this.sizeLimit();
		normalised.width = Math.min(Math.max(w, limit.minWidth), limit.maxWidth);
		normalised.height = Math.min(Math.max(h, limit.minHeight), limit.maxHeight);
		return normalised;
	},

	resizeStart: function (w, h, silent) {
		this.silent = silent;
		this.resizing = true;
		this.onResizeStart(w, h);
	},

	onResizeStart: function(w, h)
	{
		if (this.silent === false)
			this.trigger("ResizeableView:resizeStart", w, h)
	},

	resizeView: function (w, h) {
		this.resizeTargetElement().css({
			width: w,
			height: h
		});
		this.onResizeView(w, h);
	},

	onResizeView: function (w, h) {
		if (this.silent === false)
			this.trigger("ResizeableView:viewResized", w, h);
	},

	resizeStop: function (w, h) {
		this.model.setDim(w, h);
		this.resizing = false;
		this.mouseResizing = false;
		this.silent = false;
		this.onResizeStop(w, h);
	},

	onResizeStop: function (w, h) {
		if (this.silent === false)
			this.trigger("ResizeableView:resizeStop", w, h)
	},

	isResizing: function () {
		return this.resizing;
	},

	isMouseResizing: function () {
		return this.mouseResizing;
	},

	events: {
			'mousedown': '_mouseResizingStart'
	},

	setVisibility: function (value) {
		if (value)
			this.$el.removeClass('jasp-hide');
		else
			this.$el.addClass('jasp-hide');
	},

	_mouseup: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		if (self.mouseResizing) {
			var limited = self._normaliseSize(self.iW + e.pageX - self.iX, self.iH + e.pageY - self.iY);
			self.iW = limited.width;
			self.iH = limited.height;
			self.resizeStop.call(self, self.iW, self.iH);
		}
	},

	_mousemove: function (e) {
		if (!e || !e.data) return;
		var self = e.data;
		if (self.mouseResizing) {
			var limited = self._normaliseSize(self.iW + e.pageX - self.iX, self.iH + e.pageY - self.iY);
			self.iW = limited.width;
			self.iH = limited.height;
			self.resizeView(self.iW, self.iH);
			self.iX = e.pageX;
			self.iY = e.pageY;
		}
	},
});
