#version 440

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 texcoord;
layout(location = 0) out vec2 coord;

layout(std140, binding = 0) uniform buf {
  mat4  qt_Matrix;
  float qt_Opacity;
} ubuf;

out gl_PerVertex { vec4 gl_Position; };

void main()
{
    coord = texcoord;
    gl_Position = ubuf.qt_Matrix * position;
}
