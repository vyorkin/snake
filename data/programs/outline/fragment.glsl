uniform sampler2D texture;

uniform vec4 u_color;
uniform vec2 u_stepSize;

varying vec2 f_texcoord;

void main() {
  vec4 texel = texture2D(texture, f_texcoord);
  vec2 uv = f_texcoord.xy;

  vec4 color = vec4(0.0);
  float density = 0.0;

  if (texel.w >= 0.95) {
    discard;
  } else {
    for (int r = 0; r < 2; ++r) {
      density += texture2D(texture, uv + u_stepSize * vec2(float(r)) * vec2(1, 1)).w;
      density += texture2D(texture, uv + u_stepSize * vec2(float(r)) * vec2(-1, 1)).w;
      density += texture2D(texture, uv + u_stepSize * vec2(float(r)) * vec2(1, -1)).w;
      density += texture2D(texture, uv + u_stepSize * vec2(float(r)) * vec2(-1, -1)).w;
    }
    color = vec4(u_color.xyz, u_color.w * density / 8.0);
  }

  gl_FragColor = color;
}
