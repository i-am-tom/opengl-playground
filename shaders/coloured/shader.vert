#version 410 core

in vec3 vertex_position;
in vec3 vertex_colour;

uniform mat4 transformation_matrix;
uniform mat4 projection_matrix;
uniform mat4 view_matrix;

out vec3 pass_colour;

void main(void) {
  gl_Position
    = projection_matrix
    * view_matrix
    * transformation_matrix
    * vec4(vertex_position, 1);

  pass_colour = vertex_colour;
}
