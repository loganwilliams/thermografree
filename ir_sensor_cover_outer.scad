height = 1.9;
thickness = 0.5;

difference() {
    translate([-15/2, -15/2, 0]) {
        cube([15, 15, height+2.5]);
    }
    
    difference() {
        translate([-5.2, -5.2, thickness]) {
            cube([10.4, 10.4, height+2.5]);
        }
        
        translate([-5.25/2, -5.25/2, thickness]) {
            cube([5.25, 5.25, height]);
        }
    }
    
    translate([0, 0, -0.1]) {
        cylinder(r=2.25, h=10, $fn=30);
    }
}