import Shape

area : Shape -> Double
area s with (shapeView s)
  area (Triangle base height) | STriangle = 0.5 * base * height
  area (Rectangle length height) | SRectangle = length * height
  area (Circle radius) | SCircle = pi * radius * radius
