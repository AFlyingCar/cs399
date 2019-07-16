module Main

import Graphics.Rendering.Gl.Types
import Graphics.Rendering.Gl.Buffers
import Graphics.Rendering.Gl.Gl41
import Graphics.Rendering.Gl
import Graphics.Util.Glfw
import Graphics.Rendering.Config

import Pong

%include C "GL/glew.h"
%flag C "-lGLEW -lGL -lglfw"

errToStr : GLenum -> String
errToStr err = case err of
                    0 => "OK"
                    GL_INVALID_ENUM => "Invalid Enum"
                    GL_INVALID_VALUE => "Invalid Value"
                    GL_INVALID_OPERATION => "Invalid Operation"
                    GL_INVALID_FRAMEBUFFER_OPERATION => "Invalid Framebuffer Operation"
                    GL_OUT_OF_MEMORY => "Out of Memory"
                    -- GL_STACK_UNDERFLOW => "Stack Underflow"
                    -- GL_STACK_OVERFLOW => "Stack Overflow"

showError : String -> IO ()
showError msg = do
    err <- glGetError
    putStrLn $ (msg ++ " -  " ++ (show err) ++ " (" ++ (errToStr err)) ++ ")"

makeMessage : String -> String
makeMessage msg = let e = err in if e == 0
                    then msg
                    else (msg ++ " -> " ++ (show err) ++ " (" ++ (errToStr e)) ++ ")"
                    where
                      err : GLenum
                      err = unsafePerformIO (glGetError)

showAllErrors : String -> IO ()
showAllErrors msg = do
  err <- glGetError
  if err == 0
     then putStrLn $ msg ++ " - OK "
     else putStrLn $ (makeMessage msg ++ " - " ++ (show err) ++ " (" ++ (errToStr err)) ++ ")"

record Shaders where
  constructor MkShaders
  vertexShader : Int
  fragmentShader : Int
  program : Int

record Uniforms where
  constructor MkUniforms
  transform : Int

createShaders : IO( Either FileError Shaders )
createShaders = do
  glGetError
  vertexShader <- glCreateShader GL_VERTEX_SHADER

  showError "create vertex shader "
  Right vtx <- readFile "vertex.glsl" | Left err => pure (Left err)
  glShaderSource vertexShader 1 [vtx] [(cast $ length vtx)]
  glCompileShader vertexShader

  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  showError "create fragment shader "

  Right frg <- readFile "fragment.glsl" | Left err => pure (Left err)
  glShaderSource fragmentShader 1 [frg] [(cast $ length frg)]
  glCompileShader fragmentShader

  program <- glCreateProgram
  glAttachShader program vertexShader
  glAttachShader program fragmentShader
  showError "attach shaders "

  glLinkProgram program
  showError "link "
  glUseProgram program
  showError "use "

  printShaderLog vertexShader
  printShaderLog fragmentShader

  pure $ Right (MkShaders vertexShader fragmentShader program)


destroyShaders : Shaders -> IO ()
destroyShaders (MkShaders shader1 shader2 program) = do
  glGetError
  glUseProgram 0
  glDetachShader program shader1
  glDetachShader program shader2
  glDeleteShader shader1
  glDeleteShader shader2
  glDeleteProgram program
  pure ()

vertices : List (Double, Double, Double, Double)
vertices = [
    ( -0.8, -0.8, 0.0, 1.0),
    (  0.0,  0.8, 0.0, 1.0),
    (  0.8, -0.8, 0.0, 1.0)
  ]

colors : List (Double, Double, Double, Double)
colors = [
    (0.9, 0.9, 0.9, 1.0),
    (0.9, 0.9, 0.9, 1.0),
    (0.9, 0.9, 0.9, 1.0)
  ]

record Vao where
  constructor MkVao
  id : Int
  buffer1 : Int
  buffer2 : Int


flatten : List (Double, Double, Double, Double) -> List Double
flatten [] = []
flatten ((a,b,c,d) :: xs) = [a,b,c,d] ++ (flatten xs)


createBuffers : IO Vao
createBuffers = do

  ds <- sizeofDouble
  glGetError

  (vao :: _) <- glGenVertexArrays 1
  glBindVertexArray vao

  (buffer :: colorBuffer :: _) <- glGenBuffers 2
  glBindBuffer GL_ARRAY_BUFFER buffer

  let data1 = (flatten vertices)
  ptr <- doublesToBuffer data1
  glBufferData GL_ARRAY_BUFFER (ds * (cast $ length data1)) ptr GL_STATIC_DRAW
  free ptr

  showError "vertex buffer data "
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 4 GL_DOUBLE GL_FALSE 0 prim__null

  glBindBuffer GL_ARRAY_BUFFER colorBuffer

  let data2 = (flatten colors)
  ptr2 <- doublesToBuffer data2
  glBufferData GL_ARRAY_BUFFER (ds * (cast $ length data2)) ptr2 GL_STATIC_DRAW
  free ptr2

  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 4 GL_DOUBLE GL_FALSE 0 prim__null
  showError "color buffer "

  pure $ MkVao vao buffer colorBuffer

createUniforms : Int -> IO Uniforms
createUniforms program = do
  transform <- glGetUniformLocation program "transform"

  showAllErrors ("getUniform `transform` = " ++ (show transform))

  pure $ MkUniforms transform

destroyBuffers : Vao -> IO ()
destroyBuffers (MkVao vao buffer colorBuffer) = do
  glDisableVertexAttribArray 1
  glDisableVertexAttribArray 0

  glBindBuffer GL_ARRAY_BUFFER 0

  glDeleteBuffers 2 [buffer, colorBuffer]

  glBindVertexArray 0

  glDeleteVertexArrays 1 [vao]

  showError "destroy buffers "

data State = MkState GlfwWindow Vao Shaders Uniforms PongState

draw : State -> IO ()
draw (MkState win vao (MkShaders _ _ prog ) (MkUniforms transform) pong) = do
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT
    glClear GL_DEPTH_BUFFER_BIT
    glUseProgram prog
    glUniform2dv transform 1 [P1_XPOSITION, (p1_height pong)]
    showAllErrors "glUniform2dv "
    -- glUniformMatrix4dv transform 1 GL_FALSE (getPlayer1Transform pong)
    glBindVertexArray (id vao)
    glDrawArrays GL_TRIANGLES 0 3
    glfwSwapBuffers win


initDisplay : IO GlfwWindow
initDisplay = do

    glfwInit
    glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR  4
    glfwWindowHint GLFW_CONTEXT_VERSION_MINOR  1
    glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT  1
    glfwWindowHint GLFW_OPENGL_PROFILE         (toInt GLFW_OPENGL_CORE_PROFILE)

    win <- glfwCreateWindow "CS 399 - Project 3" DIMENSIONX DIMENSIONY defaultMonitor

    -- now we pretend every thing is going to be ok
    glfwMakeContextCurrent win
    glewInit
    glfwSwapInterval 0
    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LESS
    info <- glGetInfo
    putStrLn info
    pure win

eventLoop : State -> IO ()
eventLoop state@(MkState win vao shaders uniforms pong) = do
    draw state
    glfwPollEvents
    key <- glfwGetFunctionKey win GLFW_KEY_ESCAPE
    shouldClose <- glfwWindowShouldClose win
    if shouldClose || key == GLFW_PRESS
        then pure ()
        else eventLoop (MkState win vao shaders uniforms (gameLoop win pong))

main : IO ()
main = do
    win <- initDisplay
    glfwSetInputMode win GLFW_STICKY_KEYS 1
    Right shaders <- createShaders
      | Left err => printLn err
    vao <- createBuffers
    uniforms <- createUniforms (program shaders)
    eventLoop( MkState win vao shaders uniforms (MkPongState (0, 0) CENTER DEFAULT_VELOCITY CENTERY CENTERY)) -- TODO
    destroyBuffers vao
    destroyShaders shaders
    glfwDestroyWindow win
    glfwTerminate
