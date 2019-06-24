#version 130

in vec2 frag_tex;
uniform vec3 base_color_factor;
uniform sampler2D base_color_texture;

void main (void) {
  gl_FragColor = texture2D(base_color_texture, frag_tex) *
    vec4(base_color_factor, 1.0);
}
