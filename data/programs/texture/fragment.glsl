uniform sampler2D image;
uniform float u_opacity;

varying vec2 f_texcoord;

void main(void) {
  vec4 color = texture2D(image, f_texcoord);
  gl_FragColor = vec4(color.xyz, color.w * u_opacity);
}
