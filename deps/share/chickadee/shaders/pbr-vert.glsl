#version 130

in vec3 position;
in vec2 texcoord_0;
out vec2 frag_tex;
uniform mat4 mvp;

void main(void) {
  frag_tex = texcoord_0;
  gl_Position = mvp * vec4(position.xyz, 1.0);
}
