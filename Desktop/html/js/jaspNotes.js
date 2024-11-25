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

	var LineBreakClass = new Parchment.ClassAttributor('linebreak', 'linebreak', {
		scope: Parchment.Scope.BLOCK
	});

	///--- Image blots formatter ---///

	// using https://github.com/Fandom-OSS/quill-blot-formatter to format image
	Quill.register('modules/blotFormatter', QuillBlotFormatter.default);

	// custom delet behaviors becaus of https://github.com/jasp-stats/jasp-issues/issues/2776
	class _CustomDeleteAction extends QuillBlotFormatter.DeleteAction {
		constructor(formatter) {
			super(formatter);
		}

		onKeyUp = (e) => {
			if (!this.formatter.currentSpec)
				return;

			if (e.key === 'Backspace' || e.key === 'Delete') {
				const blot = Quill.find(this.formatter.currentSpec.getTargetElement());
				if (blot) 
					blot.deleteAt(0);
				this.formatter.hide();
			}
		};
	}

	class _CustomImageSpec extends QuillBlotFormatter.ImageSpec {
		constructor(formatter) {
			super(formatter);
			this.img = null;
		}

		getActions() {
			return [QuillBlotFormatter.AlignAction, QuillBlotFormatter.ResizeAction, _CustomDeleteAction];
		}

		onClick = (event) => {
			const el = event.target;

			if (!(el instanceof HTMLElement) || el.tagName !== 'IMG' || el.parentNode.parentNode.tagName === 'MJX-CONTAINER') {
				return;
			}

			this.img = el;
			this.formatter.show(this);

			const $thisOverlay = $(this.formatter.overlay);
			// auto show/hide image editor toolbar with mouse action
			$thisOverlay.on("mouseenter", () => { $thisOverlay.show() }).on("mouseleave", () => { $thisOverlay.hide() });
			$(this.img).on(	"mouseenter", () => { $thisOverlay.show() }).on("mouseleave", () => { $thisOverlay.hide() });
		};
	}
	var CustomImageSpec = _CustomImageSpec

	Quill.register('formats/linebreak', LineBreakClass);

	///--- Link blot ---///
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

    ////--- Video Blots ---////

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

    const VideoEmbed = Quill.import("blots/block/embed");

    class EmbendVideo extends VideoEmbed {
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

    ////--- Formula Blots ---////

    const FormulaEmbed = Quill.import('blots/embed');
    class FormulaBlot extends FormulaEmbed {
        static create(value) {
            let node = super.create(value);
            if (typeof value === 'string') {
                // Hack to replaces Katex with MathJax
                node = MathJax.tex2svg(value, {display: false})
                node.setAttribute('data-value', value);
            }
            return node;
        }

        static value(domNode) {
            return domNode.getAttribute('data-value');
        }

    }
    FormulaBlot.blotName = 'formula';
    FormulaBlot.className = 'ql-formula';
    FormulaBlot.tagName = 'SPAN';

    Quill.register(FormulaBlot, true);
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

        this.$el.append(`<div class="jasp-hide" data-button-class="jasp-comment"></div>`);
        this.$el.append(`<div id="editor"></div>`)
                .append(`<div class="jasp-latex-container jasp-hide">
                            <textarea class="jasp-latex-input" rows="5" cols="25" placeholder='${i18n("Input LaTeX here:")}
								&bull; ${i18n("Press `Cmd/Ctrl + Enter` to apply;")}'>
								</textarea>
                            <div class="jasp-latex-preview" title='${i18n("Click to apply formula")}'><div></div></div>
                        </div>`)

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
					specs: [CustomImageSpec], 
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

		this.$quillToolbar     = this.$el.find(".ql-toolbar")
		let quillEditorElement = this.$el.find(".ql-editor").get(0);
		
		this.$quillTooltip     = this.$el.find(".ql-tooltip");
		var quillThemeTooltip  = this.$quill.theme.tooltip;

		// Change example link from quilljs.com to a sample link
		var linkInput = quillThemeTooltip.root.querySelector('input[data-link]');
			linkInput.dataset.link = 'https://jasp-stats.org';

		let toolbarButtons = [
			{ className: '.ql-bold', 			title: i18n('Bold') },
			{ className: '.ql-italic', 			title: i18n('Italic') },
			{ className: '.ql-underline', 		title: i18n('Underline') },
			{ className: '.ql-link', 			title: i18n('Link') },
			{ className: '.ql-formula', 		title: i18n('Formula') },
			{ className: '.ql-code-block', 		title: i18n('Code Block') },
			{ className: '.ql-image', 			title: i18n('Image') },
			{ className: '.ql-video', 			title: i18n('Embed web video') },
			{ className: '.ql-header.ql-picker', title: i18n('Header') },
			{ className: '.ql-list', 			title: [i18n('Ordered List'), i18n('Unordered List')] },
			{ className: '.ql-color.ql-picker.ql-color-picker', title: i18n('Font Color') },
			{ className: '.ql-background.ql-picker.ql-color-picker', title: i18n('Background Color') },
			{ className: '.ql-script', 			title: [i18n('Subscript'), i18n('Superscript')] },
			{ className: '.ql-blockquote', 		title: i18n('Blockquote') },
			{ className: '.ql-indent', 			title: [i18n('Add Indent'), i18n('Remove Indent')] },
			{ className: '.ql-size.ql-picker', 	title: i18n('Font Size') },
			{ className: '.ql-clean', 			title: i18n('Clear Formatting') },
		   ];
		   
		   toolbarButtons.forEach((button) => {
			let elements = $(this.$quillToolbar).find(`${button.className}`);
			elements.each((index, element) => {
				let title = Array.isArray(button.title) ? button.title[index] : button.title;
				$(element).attr('title', title).tooltip( {position: {my:"center bottom-15", at:"center top"}} );
			});
		});

		// Custom mouse events for the toolbar
		this.$quillToolbar.on('mousedown', (event) => {
			event.preventDefault();
		});

		const latexContainer = this.$el.find('.jasp-latex-container')
		const latexInputBox = this.$el.find('.jasp-latex-input')
		const latexPreview = this.$el.find('.jasp-latex-preview')

		self.handleLatexEditor = {
			onEdit: function (_latexText) {
				latexContainer.removeClass('jasp-hide');
				latexInputBox.val(_latexText);
				latexInputBox.focus();
				latexPreview.tooltip({position: {my:"center bottom-15", at:"center top"}})
				_latexText.length > 0 ? latexPreview.show() : latexPreview.hide();
				const _latexSvg = MathJax.tex2svg(_latexText)
				latexPreview.get(0).replaceChildren(_latexSvg)
			},
			onSave: function (range, latexText) {
				if (self.oldBlot) {
					range = self.$quill.getSelection(true);
					range.index = self.formulaBlot.offset(self.formulaBlot.scroll)				
				} else {
					range = self.$quill.getSelection(true);
				}
				
				self.$quill.insertEmbed(range.index, 'formula', latexText, 'user');
				self.$quill.insertText(range.index + 1, ' ', 'user');
				self.$quill.setSelection(range.index + 2, 'user');

				if (self.oldBlot) {
					self.oldBlot.remove();
					self.oldBlot = null
				}
				range = null			
			},
			onClose: function () {
				latexContainer.addClass('jasp-hide');
				latexInputBox.val('');
				latexPreview.tooltip("close").empty();
			}
		}

		self.$quill.getModule('toolbar').addHandler('formula', () => {
			self.handleLatexEditor.onEdit('');
		})

		let range = null;

		latexInputBox.on("input", (e) => {
			self.handleLatexEditor.onEdit(e.target.value)
		})

		latexInputBox.on("blur", (e) => {
			if(e.relatedTarget && !e.relatedTarget.parentNode.className === 'jasp-latex-preview')
				return;
			const latexText = latexInputBox.val();
			self.handleLatexEditor.onSave(range, latexText);
			self.formulaClicked = false;
			self.handleLatexEditor.onClose();
		})

		latexInputBox.on("keydown", (e) => {
			if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
				e.preventDefault();
				latexInputBox.blur();
			} 
				
			if (e.key === 'Escape') {
				self.handleLatexEditor.onClose();
			}
		});

		function setContainerPosition() {
			let _range = self.$quill.getSelection()
			let _editorBounds = self.$quill.getBounds(_range ? _range.index : 0);

			let containerPosition = {
				top: _editorBounds.top + 40,
				left: _editorBounds.left + 20
			};
			latexContainer.css({
				top: `${containerPosition.top}px`,
				left: `${containerPosition.left}px`
			});
		}

		self.$quill.on('editor-change', () => {
			$(".ui-tooltip-content").parents('div').remove(); // Remove jquery-ui tooltips legacy,bug:https://stackoverflow.com/questions/19266886/tooltip-not-disappearing
			let $formulaNode = this.$el.find('.ql-editor mjx-container')
				$formulaNode.attr('title', i18n('Click to edit this formula'))
							.tooltip({position: {my:"center bottom-15", at:"center top"}});

			$formulaNode.on('click dblclick', (e) => {
				if (e.type === 'dblclick')
					return; // Make sure that click and double clicks do not interfere with each other

				self.formulaClicked = true
				let currentFormula = e.currentTarget
				self.formulaBlot = window.Quill.find(currentFormula);
				const _currentLatex = currentFormula.getAttribute("data-value").trim();
				self.handleLatexEditor.onEdit(_currentLatex)
				self.oldBlot = self.formulaBlot // Get legacy formula range to remove while save
			});

		});
		
		quillEditorElement.addEventListener('focusin', () => {
			self.setQuillToolbarVisibility('block');
		});

		quillEditorElement.addEventListener('focusout', () => {
			if (!latexContainer.hasClass('jasp-hide') || this.$quillTooltip.is(':visible') || self.formulaClicked)
				return;
			self.setQuillToolbarVisibility('none');
		});

		if (this.model.get('deltaAvailable')) {
			delt = this.model.get('delta');
		} else {
			if (this.model.get('format') === 'markdown') {
				this.model.toHtml();
				html = this.model.get("text");
			}

			delt = this.$quill.clipboard.convert({"html" : html});    //see:https://github.com/slab/quill/issues/4135
		}

		this.$quill.setContents(delt);
		self.onNoteChanged(self.$quill.root.innerHTML, self.$quill.getContents());

		this.$quill.on('text-change', function(delta, oldDelta, source) {

			setContainerPosition();

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

		this.$quillToolbar.css("display", display);

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
