#version 410 core

in vec3 vertex_position;
in vec3 vertex_colour;

out vec3 pass_colour;

void main(void) {
  gl_Position = vec4(vertex_position, 1);
  pass_colour = vertex_colour;
}
