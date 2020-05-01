uniform sampler2D texture;

varying vec2 f_texcoord;

void main(void) {
  vec4 color = texture2D(texture, f_texcoord);
  gl_FragColor = color;
}
