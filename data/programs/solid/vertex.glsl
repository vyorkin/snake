attribute vec2 coord2d;

void main(void) {
  gl_Position
    = gl_ModelViewMatrix
    * vec4(coord2d, 0.0, 1.0);
}
