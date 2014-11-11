#version 330
in vec2 UV;
out vec4 fragColor;
uniform sampler2D tex;
uniform vec4 color;
uniform bool useTexture;
void main(void){
  if(useTexture){
    fragColor = texture( tex, UV ).rgba * color;
  }else{
    fragColor = color;
  }
}