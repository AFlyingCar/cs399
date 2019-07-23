module Main

import Data.Matrix.Algebraic

import Graphics.Rendering.Gl.Types
import Graphics.Rendering.Gl.Buffers
import Graphics.Rendering.Gl.Gl41
import Graphics.Rendering.Gl
import Graphics.Util.Glfw
import Graphics.Rendering.Config

-- Vect
import Data.Vect
import Data.Matrix
-- translate
import Graphics.Util.Transforms

import Pong

%include C "GL/glew.h"
%flag C "-lGLEW -lGL -lglfw"

split_verts : VertList
split_verts = [
  (0.0, 0.0, 0.0, 1.0),
  (0.05, 0.0, 0.0, 1.0),
  (0.0, 1, 0.0, 1.0),
  (0.05, 1, 0.0, 1.0)
  ]

-- line_transform: TransformationMatrix
-- line_transform = 

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

elements : List Int
elements = [ 0, 1, 2, 1, 3, 2 ]

colors : List (Double, Double, Double, Double)
colors = [
    (0.9, 0.9, 0.9, 1.0),
    (0.9, 0.9, 0.9, 1.0),
    (0.9, 0.9, 0.9, 1.0),
    (0.9, 0.9, 0.9, 1.0)
  ]

record Vao where
  constructor MkVao
  id : Int
  buffer1 : Int
  buffer2 : Int
  ebuffer : Int


flatten : List (Double, Double, Double, Double) -> List Double
flatten [] = []
flatten ((a,b,c,d) :: xs) = [a,b,c,d] ++ (flatten xs)

populateVAO : Int -> List (Double, Double, Double, Double) -> IO Vao
populateVAO vao verts = do
  ds <- sizeofDouble
  glGetError

  glBindVertexArray vao

  (buffer :: elementBuffer :: colorBuffer :: _) <- glGenBuffers 3

  let data1 = (flatten verts)
  ptr <- doublesToBuffer data1
  glBindBuffer GL_ARRAY_BUFFER buffer
  glBufferData GL_ARRAY_BUFFER  (ds * (cast $ length data1)) ptr GL_STATIC_DRAW
  free ptr

  eptr <- intsToBuffer elements
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER elementBuffer
  glBufferData GL_ELEMENT_ARRAY_BUFFER (ds * (cast $ length elements)) eptr GL_STATIC_DRAW
  free eptr

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

  pure $ MkVao vao buffer colorBuffer elementBuffer

data TripleVao = MkTriple Vao Vao Vao

createBuffers : IO TripleVao
createBuffers = do
  (vao :: pvao :: svao :: _) <- glGenVertexArrays 3
  MkTriple <$> (populateVAO vao vertices) <*> (populateVAO pvao puck_verts) <*> (populateVAO svao split_verts)

createUniforms : Int -> IO Uniforms
createUniforms program = do
  transform <- glGetUniformLocation program "transform"

  showAllErrors ("getUniform `transform` = " ++ (show transform))

  pure $ MkUniforms transform

destroyBuffers : Vao -> IO ()
destroyBuffers (MkVao vao buffer colorBuffer ebuffer) = do
  glDisableVertexAttribArray 1
  glDisableVertexAttribArray 0

  glBindBuffer GL_ARRAY_BUFFER 0

  glDeleteBuffers 3 [buffer, colorBuffer, ebuffer]

  glBindVertexArray 0

  glDeleteVertexArrays 1 [vao]

  showError "destroy buffers "

-- Window PaddleVAO PuckVAO Shaders Uniforms GameState
data State = MkState GlfwWindow Vao Vao Vao Shaders Uniforms PongState

draw : State -> IO ()
draw (MkState win vao pvao svao (MkShaders _ _ prog ) (MkUniforms transform) pong) = do
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT
    glClear GL_DEPTH_BUFFER_BIT
    glUseProgram prog

    -- Draw Player1
    glUniformMatrix4fv transform 1 GL_FALSE (toList (toGl (getPlayer1Transform pong)))
    glBindVertexArray (id vao)
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT prim__null

    -- Draw Player2
    glUniformMatrix4fv transform 1 GL_FALSE (toList (toGl (getPlayer2Transform pong)))
    glBindVertexArray (id vao)
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT prim__null

    -- Draw the Puck
    glUniformMatrix4fv transform 1 GL_FALSE (toList (toGl (getPuckTransform pong)))
    glBindVertexArray (id pvao)
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT prim__null

    -- Draw the line splitting the center of the screen
    -- glUniformMatrix4fv transform 1 GL_FALSE (toList (toGl (translate [0.5, 0.0, 0.0, 1.0])))
    -- glBindVertexArray (id svao)
    -- glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT prim__null

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

scoresToString: PongState -> String
scoresToString (MkPongState (p1_score, p2_score) puck p1 p2) = "Player 1 -- " ++ (show p1_score) ++ "\nPlayer 2 -- " ++ (show p2_score) ++ "\nThanks for playing!"

eventLoop : State -> IO ()
eventLoop state@(MkState win vao pvao svao shaders uniforms pong) = do
    draw state
    glfwPollEvents
    key <- glfwGetFunctionKey win GLFW_KEY_ESCAPE
    shouldClose <- glfwWindowShouldClose win
    if shouldClose || key == GLFW_PRESS
        then putStrLn (scoresToString pong)
        else eventLoop (MkState win vao pvao svao shaders uniforms (gameLoop win pong))

main : IO ()
main = do
    win <- initDisplay
    glfwSetInputMode win GLFW_STICKY_KEYS 1
    Right shaders <- createShaders
      | Left err => printLn err
    (MkTriple vao pvao svao) <- createBuffers
    uniforms <- createUniforms (program shaders)
    paddle1 <- pure $ MkGameObject vertices P1_INIT_POSITION
    paddle2 <- pure $ MkGameObject vertices P2_INIT_POSITION
    puck <- pure $ MkPuck (MkGameObject puck_verts PUCK_INIT_POSITION) PUCK_INIT_VELOCITY
    eventLoop( MkState win vao pvao svao shaders uniforms (MkPongState (0, 0) puck paddle1 paddle2)) -- TODO
    destroyBuffers vao
    destroyShaders shaders
    glfwDestroyWindow win
    glfwTerminate
