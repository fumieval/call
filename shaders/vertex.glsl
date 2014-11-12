#version 330
uniform mat4 projection;
uniform mat4 matrices[13];
uniform int level;
uniform int useEnv;
out vec2 UV;
out vec2 envUV;
in vec3 in_Position;
in vec2 in_UV;
in vec3 in_Normal;

void main(void) {
  mat4 model = mat4(1.0);
  for(int i=0; i < level; i++)
  {
    model *= matrices[i];
  }
  gl_Position = projection * model * vec4(in_Position, 1.0);
  UV = in_UV;
  if(useEnv != 0)
  {
    vec3 r = reflect( normalize(vec3(model * vec4(in_Position, 1.0))), in_Normal );
    float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );
    envUV = r.xy / m + 0.5;
  }
}