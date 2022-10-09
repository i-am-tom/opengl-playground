#version 410 core

in vec3 vertex_position;
in vec2 texture_coordinate;

out vec2 pass_texture_coordinate;
// out vec3 colour;

void main(void) {
  gl_Position = vec4(vertex_position, 1);
  // colour = vec3(vertex_position.x + 0.5, 1, vertex_position.y + 0.5);
  pass_texture_coordinate = texture_coordinate;
}
