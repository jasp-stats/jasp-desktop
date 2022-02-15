import QtQuick 2.15

///Setting src to an Image will put this image inverter there and enable/disable it based on a dark theme is enabled. This is especially useful for getting the negative of a plot
ShaderEffect
{
	enabled:				jaspTheme.isDark
	visible:				jaspTheme.isDark
	blending:				false
	anchors.fill:			src
	z:						3

	property variant src:	plotImg

	vertexShader:	"qrc:/components/JASP/Widgets/ImageInverter.vert.qsb"
	fragmentShader: "qrc:/components/JASP/Widgets/ImageInverter.frag.qsb"
}
