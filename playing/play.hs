import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  mainLoop

display = do
  clear [ColorBuffer]
  color $ Color3 (1.0::GLfloat) 0 0
  renderPrimitive Triangles $ mapM_ (\(x, y, z)->vertex$Vertex3 (0.9*x) (0.9*y) (0.9*z)) myPoints
  flush

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]

