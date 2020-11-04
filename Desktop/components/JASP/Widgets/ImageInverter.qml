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

	vertexShader: "
		uniform		highp mat4 qt_Matrix;
		attribute	highp vec4 qt_Vertex;
		attribute	highp vec2 qt_MultiTexCoord0;
		varying		highp vec2 coord;

		void main() {
			coord = qt_MultiTexCoord0;
			gl_Position = qt_Matrix * qt_Vertex;
		}"

	fragmentShader: "
	varying highp	vec2		coord;
	uniform			sampler2D	src;
	uniform lowp	float		qt_Opacity;

	void main() {
		highp vec4 tex = texture2D(src, coord);
		gl_FragColor = vec4(vec3(1.0) - (tex.rgb), 1.0f) * tex.a * qt_Opacity;
	}"
}
