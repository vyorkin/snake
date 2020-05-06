uniform vec4 color;
uniform float u_opacity;

void main(void) {
  gl_FragColor = vec4(color.xyz, color.w * u_opacity);
}
