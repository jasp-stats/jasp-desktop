JASPWidgets.images = Backbone.Collection.extend({

	model: JASPWidgets.image,
});

JASPWidgets.imagesView = JASPWidgets.CollectionView.extend({

	createItemView: function (item) {
		var imageView = new JASPWidgets.imageView({ className: "jasp-images-image jasp-image", model: item });
		this.listenTo(imageView.resizer, "ResizeableView:resizeStart", this.onResizingStart);
		return imageView;
	},

	onResizingStart: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (imageView.resizer.isMouseResizing()) {
				this.listenTo(imageView.resizer, "ResizeableView:resized", this.onResized);
				this.listenTo(imageView.resizer, "ResizeableView:viewResized", this.onViewResized);
			}
			else
				imageView.resizer.resizeStart(w, h, true);
		}
	},

	onViewResized: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (!imageView.resizer.isMouseResizing())
				imageView.resizer.resizeView(w, h);
		}
	},

	onResized: function (w, h) {
		for (var i = 0; i < this.views.length; i++) {
			var imageView = this.views[i];
			if (imageView.resizer.isMouseResizing()) {
				this.stopListening(imageView.resizer);
			}
			else
				imageView.resizer.resizeStop(w, h);
		}
	}
});