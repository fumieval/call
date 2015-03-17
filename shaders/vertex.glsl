#version 150
uniform mat4 projection;
uniform mat4 matrices[13];
uniform int level;

in vec3 in_Position;
in vec2 in_UV;
in vec3 in_Normal;

out vec2 texUV;
out vec3 normal;
out vec3 lightDir;

void main(void) {
  mat4 model = mat4(1.0);
  for(int i=0; i < level; i++)
  {
    model *= matrices[i];
  }
  gl_Position = projection * model * vec4(in_Position, 1.0);
  texUV = in_UV;
  normal = in_Normal;
  lightDir = vec3(0.0, 0.0, 0.0);
}
