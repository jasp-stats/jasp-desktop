
var videoUrlList = [];

window.sendUrlWhitelist = function (RequestedURL) {
	videoUrlList = RequestedURL; // to get from MainPage.qml
}

function isURLInWhitelist(hostname) {
	for (let i = 0; i < videoUrlList.length; i++) {
		let pattern = videoUrlList[i];
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
		// enable it once h.264 encoding is available on qtwebengine
		// if (matchs = url.match(/^(?:(https?):\/\/)?(?:www\.)?vimeo\.com\/(\d+)/)) {
		// 	return (match[1] || 'https') + '://player.vimeo.com/video/' + matchs[2] + '/';
		// }
		// if (matchs = url.match(/(?:www\.|\/\/)bilibili\.com\/video\/(\w+)/)) {
		// 	return 'https://player.bilibili.com/player.html?bvid=' + matchs[1]

		// }
		// if (matchs = url.match(/\/\/v\.qq\.com\/x\/cover\/.*\/([^\/]+)\.html\??.*/)) {
		// 	return 'https://v.qq.com/txp/iframe/player.html?vid=' + matchs[1]
		// }
		
		return url
	}

	const BlockEmbed = Quill.import("blots/block/embed");

	class EmbendVideo extends BlockEmbed {
		static create(value) {
			value = customVideoUrl(value)
			let node = super.create(value);
			let div = document.createElement('div');
				div.setAttribute("title", i18n("Unsupported video services"));
			$(div).append(`${i18n('JASP only allows the following videoservices:')}<br><br> <i>"Youtube video"</i> <br><br>${i18n('Contact the JASP team to request adding another videoservice to the list.')}`)	
			node.setAttribute('frameborder', '0');
			node.setAttribute('allowfullscreen', true);
			node.setAttribute('src', value);
			if (!isURLInWhitelist((new URL(value).hostname))) {
				node.innerHTML = $(div).dialog({ // give a warnning for unsupported urls and then remove from node.
					modal: true, buttons: {
						Ok: function () {
							$(node).remove();
							$(this).dialog("close");
						}
					}
				})
			}
			return node;
		}
		
		static value(node){
			return node.getAttribute('src');
		}
	}

	EmbendVideo.blotName = 'video';
	EmbendVideo.className = 'ql-video';
	EmbendVideo.tagName = 'IFRAME';

	Quill.register(EmbendVideo, true);

}


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

			let $blotResizer = this.$el.find('.blot-formatter__overlay');
			let $resizeHandles = this.$el.find('[class^="blot-formatter"]');

			// auto show/hide resizer handles while hover/leave.
			$blotResizer.on("mouseenter", (event) => {
				if (event.relatedTarget.parentNode.tagName === "SPAN") //do not use resizer for formula 'mjx-container -> span -> img'
					$resizeHandles.hide()
				else
					$resizeHandles.show()
			}).on("mouseleave", () => {
				$resizeHandles.hide()
			});

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
			let _quillRootHTML = self.$quill.root

			function hasFormula(obj) {
				for (let key in obj) {
					if (typeof obj[key] === 'object') {
						if (hasFormula(obj[key])) {
							return true;
						}
					} else if (key === 'formula') {
						return true;
					}
				}
				return false;
			}

			if (hasFormula(delta)) {
				const svgToPng = new SvgToPng();
				svgToPng.convert(self.$quill.root);
			}
			self.onNoteChanged(_quillRootHTML.innerHTML, self.$quill.getContents());
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
