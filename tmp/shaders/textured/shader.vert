#version 410 core

in vec3 vertex_position;
in vec2 texture_coordinate;

out vec2 pass_texture_coordinate;

void main(void) {
  gl_Position = vec4(vertex_position, 1);
  pass_texture_coordinate = texture_coordinate;
}
