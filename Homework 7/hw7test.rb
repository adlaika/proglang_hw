
require "./hw7.rb"

#Testing below
$d = LineSegment.new(1.0,2.0,-3.0,-4.0)
if not ($d.eval_prog([]) == $d)
  puts "LineSegement eval_prog should return self"
end

$d4 = $d.intersect(LineSegment.new(-3.0,-4.0,1.0,2.0))
#should intersect, and print nothing