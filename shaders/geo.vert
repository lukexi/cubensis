#version 330 core

uniform mat4 uMVP;
uniform mat4 uModel;

in vec3 aPosition;
in vec3 aNormal;

out vec3 vNormal;
out vec3 vPosition;

void main( void ) { 

  gl_Position = uMVP * vec4(aPosition, 1.0);

  vPosition = vec3(uModel * vec4(aPosition, 1.0));
  vNormal   = vec3(uModel * vec4(aNormal, 0.0));

}
