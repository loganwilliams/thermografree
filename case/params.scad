module roundedRect(size, radius)
    {
    x = size[0];
    y = size[1];
    z = size[2];

    linear_extrude(height=z)
    hull()
    {
        // place 4 circles in the corners, with the given radius
        translate([radius, radius, 0])
        circle(r=radius);

        translate([x - radius, radius, 0])
        circle(r=radius);

        translate([radius, y - radius, 0])
        circle(r=radius);

        translate([x - radius, y - radius, 0])
        circle(r=radius);
    }
}

module roundedPoly(size, radius)
{
	x = size[0];
	y = size[1];
	z = size[2];

	hull()
	{
		// place 4 circles in the bottom corners, with the given radius
		translate([radius, radius, radius])
		sphere(r=radius);
	
		translate([x - radius, radius, radius])
		sphere(r=radius);
	
		translate([radius, y - radius, radius])
		sphere(r=radius);
	
		translate([x - radius, y - radius, radius])
		sphere(r=radius);
        
        // place 4 circles in the top corners, with the given radius
		translate([radius, radius, z - radius])
		sphere(r=radius);
	
		translate([x - radius, radius, z - radius])
		sphere(r=radius);
	
		translate([radius, y - radius, z - radius])
		sphere(r=radius);
	
		translate([x - radius, y - radius, z - radius])
		sphere(r=radius);
	}
}

module standoff(radius, height, inner_radius, depth=10) {
    difference() {
        cylinder(h=height, r=radius, $fn=40);
        translate([0, 0, height-20]) {
            cylinder(h=20.1, r=inner_radius, $fn=40);
        }
    }
}
    

left_margin = 10;
right_margin = 30;
top_margin = 25;
bottom_margin = 34;
screen_width = 118.1;
screen_height = 70.9;
height = 2;

total_width = left_margin+right_margin+screen_width;
total_height = top_margin+bottom_margin+screen_height;

ribbon_cable_width=13;

wall_thickness = 2;
depth = 50;
standoff_od = 4;

camera_offset_x = 105;
camera_offset_y = 12;
camera_standoff_height = 4.7;

sensor_offset_x = 135;
sensor_offset_y = 15;