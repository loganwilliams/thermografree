include <params.scad>

roundedRect([10, 10, 20], 2);

translate([20, 0, 0]) {
roundedPoly([10, 10, 20], 2, $fn=40); }

translate([-20, 0, 0]) {
cube([10, 10, 20]); }