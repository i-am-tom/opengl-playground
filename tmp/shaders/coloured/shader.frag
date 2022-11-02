#version 410 core

in vec3 pass_colour;
out vec4 out_Color;

void main(void) {
  out_Color = vec4(pass_colour, 1.0);
}
