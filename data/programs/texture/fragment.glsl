uniform sampler2D image;

varying vec2 f_texcoord;

void main(void) {
  vec4 color = texture2D(image, f_texcoord);
  gl_FragColor = color;
}
