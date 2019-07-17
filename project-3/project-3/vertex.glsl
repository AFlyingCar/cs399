#version 410 core

layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;

uniform mat4 transform;

out vec4 ex_Color;

void main(void)
{
  gl_Position = transform * in_Position;
  ex_Color = in_Color; // vec4((transform * in_Position).rgb, 1.0);
}
