#version 330
in vec2 UV;
in vec2 envUV;
in vec4 vColor;
out vec4 fragColor;
uniform vec4 color;
uniform bool useTexture;
uniform int useEnv;
uniform sampler2D tex;
uniform sampler2D env;

void main(void){
  if(useTexture){
    fragColor = texture( tex, UV ).rgba * vColor * color;
  }else{
    fragColor = vColor * color;
  }
  if (useEnv == 1)
  {
    fragColor += texture( env, envUV ).rgba;
  }
  else if (useEnv == 2)
  {
    fragColor *= texture( env, envUV ).rgba;
  }
}