#version 330 core
uniform mat4 projection;
uniform mat4 matrices[13];
uniform int level;
uniform bool useEnv;
out vec2 UV;
out vec2 envUV;
in vec3 in_Position;
in vec2 in_UV;
in vec3 in_Normal;
out vec3 lightDir;
out vec3 normal;

void main(void) {
  mat4 model = mat4(1.0);
  for(int i=0; i < level; i++)
  {
    model *= matrices[i];
  }
  gl_Position = projection * model * vec4(in_Position, 1.0);
  UV = in_UV;

  if(useEnv)
  {
    vec3 refl = reflect( normalize(vec3(model * vec4(in_Position, 1.0))), in_Normal );
    float m = 2.0 * sqrt( refl.x*refl.x + refl.y*refl.y + (refl.z+1.0)*(refl.z+1.0) );
    envUV = refl.xy / m + 0.5;
  }
  normal = in_Normal;
  lightDir = vec3(0.0);
}