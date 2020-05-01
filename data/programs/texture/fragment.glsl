uniform sampler2D texture;

uniform vec3  u_invGamma;
uniform float u_opacity;

varying vec2 f_texcoord;

void main(void) {
  vec4 color = texture2D(texture, f_texcoord);
  // gl_FragColor = color;

  gl_FragColor = vec4(
    pow(color.xyz, u_invGamma),
    color.w * u_opacity
  );
}
