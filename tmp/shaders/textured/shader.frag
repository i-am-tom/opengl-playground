#version 410 core

in vec2 pass_texture_coordinate;
uniform sampler2D texture_sampler;

out vec4 out_Color;

void main(void) {
  out_Color = texture (texture_sampler, pass_texture_coordinate);
}
