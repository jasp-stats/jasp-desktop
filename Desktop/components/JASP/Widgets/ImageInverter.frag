#version 440

layout(location=0) in   vec2 coord;
layout(location=0) out  vec4 fragColor;


layout(std140, binding = 0) uniform buf
{
    mat4 qt_Matrix;
    float qt_Opacity;
} ubuf;

layout(binding=1) uniform   sampler2D src;



void main(void)
{
    vec4 tex = texture(src, coord);
    fragColor = vec4(vec3(1.0) - (tex.rgb), 1.0) * tex.a * ubuf.qt_Opacity;
}
