#version 400
out vec4 fragColor;

// Texture
in vec2 texUV;

in vec3 normal;
// in vec3 lightDir;
// in float fogginess;

uniform sampler2D tex;

uniform vec4 diffuse;
uniform float textureMix;
uniform vec4 fogColor;

void main(void){
  // vec3 n = mix(normal, texture(normalMap, texUV).rgb * 2 - 1, normalMix);

  vec4 d = texture(tex, texUV).rgba * diffuse;

  // vec3 view = vec3(0,0,1);
  // float brightness = 1.0;

  // float shine = dot(reflect(lightDir, n), view);

  // fragColor = vec4(d.rgb * brightness + specular * shine, d.a);
  fragColor = d;

}
