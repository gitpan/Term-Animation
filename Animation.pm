package Term::Animation;

use 5.006;
use strict;
use warnings;
use Carp;
use Curses;

use Data::Dumper;

our $VERSION = '1.1';

our ($FAILURE, $SUCCESS) = (0, 1);
our ($color_names, $color_ids) = color_list();

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self  = {};

  $self->{OBJECTS} = {};

  $self->{WIN} = shift;
  if(defined($self->{WIN})) {
    unless(ref($self->{WIN}) eq 'Curses::Window') {
      carp("Expecting Curses::Window object, recieved " . ref($self->{WIN}));
      return undef;
    }
  }
  else {
    $self->{WIN} = new Curses;
    noecho();
    curs_set(0);
  }

  ($self->{WIDTH}, $self->{HEIGHT}, $self->{ASSUMED_SIZE}) = get_term_size($self->{WIN});
  bless ($self, $class);
  return $self;
}

##################### COLOR UTILITIES #######################

# create lists mapping full color names (eg. 'blue') and
# single character color ids (eg. 'b')
sub color_list {
  my %color_n;
  my %color_i = (
	black	=> 'k',
	white	=> 'w',
	red	=> 'r',
	green	=> 'g',
	blue	=> 'b',
	cyan	=> 'c',
	magenta	=> 'm',
	yellow	=> 'y',
  );

  for (keys %color_i) {
    $color_i{uc($_)} = uc($color_i{$_});
  }

  for (keys %color_i) {
    $color_n{$color_i{$_}} = $_;
    $color_n{$_} = $_;
    $color_n{uc($_)} = uc($_);
  }

  for(qw{ k w r g b c m y }) {
    $color_i{$_} = $_;
    $color_i{uc($_)} = uc($_);
  }

  return (\%color_n, \%color_i);
}

# build a list of every color combination for our current
# background color
sub set_colors {
  my ($self) = @_;

  my $cid = 1;

  my $bg = eval "Curses::COLOR_$self->{BG}";

  for my $f ('w', 'r', 'g', 'b', 'c', 'm', 'y', 'k') {
    my $c = uc(color_name($f));
    init_pair($cid, eval "Curses::COLOR_$c", $bg);
    $self->{COLORS}{$f} = COLOR_PAIR($cid);
    $cid++;
  }
}

# return the full name of a color, given either a full
# name or a single char. abbreviation
sub color_name {
  my ($color) = @_;
  if(defined($color_names->{$color})) {
    return $color_names->{$color};
  }
  else {
    carp("Attempt to allocate unknown color: $color");
    return undef;
  }
}

# return the single char. abbreviation for a color, 
# given either a full name or abbreviation
sub color_id {
  my ($color) = @_;
  if(defined($color_ids->{$color})) {
    return $color_ids->{$color};
  }
  else {
    carp("Attempt to allocate unknown color: $color");
    return undef;
  }
}

sub is_valid_color {
  my ($color) = @_;
  return(defined($color_ids->{$color}));
}

sub color_enabled {
  my ($self) = @_;
  return $self->{COLOR};
}

# turn on ANSI color, and initialize color settings
sub enable_color {
  my ($self) = @_;
  return if($self->{COLOR});
  start_color();
  unless(defined($self->{BG})) { $self->{BG} = 'BLACK'; }
  $self->set_colors();
  $self->{WIN}->bkgdset($self->{COLORS}{'w'});
  $self->{COLOR} = 1;
}

# turn off color
sub disable_color {
  my ($self) = @_;
  return unless($self->{COLOR});
  $self->{COLOR} = 0;
}

# update the background color. accepts either a full
# color name ('blue') or a single character name ('b')
sub set_background {
  my ($self, $color) = @_;
  my $bg_color = color_name($color);
  if(defined($bg_color)) {
    $self->{BG} = uc($bg_color);
    $self->set_colors();
    $self->{WIN}->bkgdset($self->{COLORS}{'w'});
    return $SUCCESS;
  }
  else {
    return $FAILURE;
  }
}

########## END COLOR UTILITIES ###########

# perform a single animation cycle
sub animate {
  my ($self) = @_;
  $self->do_callbacks();
  $self->build_screen();
  $self->display_screen();
}

# return the size of the screen, and whether or
# not we had to guess
sub get_screen_size {
  my $self = shift;
  return($self->{WIDTH}+1, $self->{HEIGHT}+1, $self->{ASSUMED_SIZE});
}

# resize our curses window
sub update_term_size {
  my $self = shift;
  # dunno how portable this is. i should probably be using
  # resizeterm.
  endwin();
  refresh();
  ($self->{WIDTH}, $self->{HEIGHT}, $self->{ASSUMED_SIZE}) = get_term_size($self->{WIN});
}

# try to figure out the terminal size, and set
# a reasonable size if we can't. the 'assumed_size'
# variable will let programs know if we had to
# guess or not.
sub get_term_size {
  my $win = shift;
  my ($width, $height, $assumed_size);
  # find the width and height of the terminal
  $width = $win->getmaxx();
  $height = $win->getmaxy();
  if($width and $height) {
    $assumed_size = 0; # so we know if we can limit the max size or not
  }
  else {
    $assumed_size = 1;
    $width = 80;
    $height = 24;
  }
  return($width-1, $height-1, $assumed_size);
}

# write to the curses window
sub build_screen {
  my($self) = @_;

  # clear the window before we start redrawing
  $self->{WIN}->addstr( 0, 0, ' 'x$self->size() );

  foreach my $object_name (sort {$self->{OBJECTS}->{$a}{'Z'} <=> $self->{OBJECTS}->{$b}{'Z'}} keys %{$self->{OBJECTS}}) {
    draw_object($self, $object_name);
  }
}

# under construction
# haven't decided for sure yet how i want to implement this
sub draw_extending_object {
  my ($self, $object_name) = @_;

  my $shape = $self->{OBJECTS}->{$object_name}{SHAPE}[$self->{OBJECTS}->{$object_name}{CURR_FRAME}];
  my ($x, $y) = ($self->{OBJECTS}->{$object_name}{'X'}, $self->{OBJECTS}->{$object_name}{'Y'});
  my ($w, $h) = ($self->{WIDTH}, $self->{HEIGHT});
  my ($exp, $exn, $eyp, $eyn) = ($self->{OBJECTS}{$object_name}{EXTEND_X_POS},
                                $self->{OBJECTS}{$object_name}{EXTEND_X_NEG},
                                $self->{OBJECTS}{$object_name}{EXTEND_Y_POS},
                                $self->{OBJECTS}{$object_name}{EXTEND_Y_NEG});
  if($eyp) {

  }
  elsif($eyn) {

  }  
}

# draw an object into the curses window in memory
sub draw_object {
  my ($self, $object_name) = @_;

  # a few temporary variables to make the code below easier to read
  my $shape   = $self->{OBJECTS}{$object_name}{SHAPE}[$self->{OBJECTS}{$object_name}{CURR_FRAME}];
  my $colors  = $self->{COLORS};
  my $fg      = $self->{OBJECTS}{$object_name}{COLOR}[$self->{OBJECTS}{$object_name}{CURR_FRAME}];
  my $attrs   = $self->{OBJECTS}{$object_name}{ATTR}[$self->{OBJECTS}{$object_name}{CURR_FRAME}];
  my ($x, $y) = ($self->{OBJECTS}{$object_name}{'X'}, $self->{OBJECTS}{$object_name}{'Y'});
  my ($w, $h) = ($self->{WIDTH}, $self->{HEIGHT});
  my $wrap    = $self->{OBJECTS}{$object_name}{WRAP};
  my $trans   = $self->{OBJECTS}{$object_name}{TRANSPARENT};
  my $color   = $self->{COLOR};
  my $attr;

  for my $i (0..$#{$shape}) {

    my $y_pos = $y+$i;

    for my $j (0..$#{$shape->[$i]}) {

      unless($shape->[$i][$j] eq $trans) { # transparent char

        my $x_pos = $x+$j;

        if($wrap) {
          while($x_pos > $w) { $x_pos -= ($w + 1); }
          while($y_pos > $h) { $y_pos -= ($h + 1); }
        }
        elsif($x_pos > $w or $y_pos > $h) {
          next;
        }

        unless($x_pos < 0 or $y_pos < 0) {
          if($color) {

            if(defined($attrs->[$i][$j])) {
              $attr = $colors->{$fg->[$i][$j]} | $attrs->[$i][$j];
            }
            else {
              $attr = $colors->{$fg->[$i][$j]};
            }

            $self->{WIN}->attron( $attr );
            $self->{WIN}->addstr( int($y_pos), int($x_pos), $shape->[$i][$j]);
            $self->{WIN}->attroff( $attr );

          }
          else {
            $self->{WIN}->addstr( int($y_pos), int($x_pos), $shape->[$i][$j]);
          }
        }

      }
    }
  }
}

# add an object to the animation. if it has the same name
# as an existing object, the old object will be replaced
sub add_object {
  my ($self, @objects) = @_;
  foreach my $object (@objects) {
    $self->{OBJECTS}{$object->{NAME}} = $object;
  }
}

# remove an object from the animation. 
sub del_object {
  my ($self, $object_name) = @_;
  if(defined($self->{OBJECTS}{$object_name})) {
    if(defined($self->{OBJECTS}{$object_name}{DEATH_CB})) {
      $self->{OBJECTS}{$object_name}{DEATH_CB}->($self, $object_name);
    }
    delete $self->{OBJECTS}{$object_name};
    return $SUCCESS;
  }
  else {
    carp("Attempted to destroy nonexistant object '$object_name'");
    return $FAILURE;
  }
}

# remove every object from the animation
sub remove_all_objects {
  my ($self) = @_;
  $self->{OBJECTS} = {};
}

# ask for the current frame number of an object
sub get_current_frame {
  my ($self, $object) = @_;
  if(defined($self->{OBJECTS}{$object})) {
    return($self->{OBJECTS}{$object}{CURR_FRAME});
  }
  else {
    carp("Frame number requested for nonexistant object '$object'");
  }
}

# ask for the position of an object
sub get_position {
  my ($self, $object) = @_;
  if(defined($self->{OBJECTS}{$object})) {
    return ($self->{OBJECTS}{$object}{X},
           $self->{OBJECTS}{$object}{Y},
           $self->{OBJECTS}{$object}{Z});
  }
  else {
    carp("Position requested for nonexistant object '$object'");
    return(undef, undef, undef);
  }
}

# ask if an object exists or not
sub exist {
  my ($self, $object) = @_;
  return defined($self->{OBJECTS}{$object});
}

# get the width of the screen
sub width {
  my ($self) = @_;
  return $self->{WIDTH} + 1;
}

# get the height of the screen
sub height {
  my ($self) = @_;
  return $self->{HEIGHT} + 1;
}


sub size {
  my ($self) = @_;
  return ( ( $self->{HEIGHT} + 1 ) * ( $self->{WIDTH} + 1 ) )
}


# redraw the entire screen
sub redraw_screen {
  my ($self) = @_;
  $self->{WIN}->clear();
  $self->{WIN}->refresh();
  $self->build_screen();
  $self->{WIN}->move($self->{HEIGHT}, $self->{WIDTH});
  $self->{WIN}->refresh();
}

# draw the elements of the screen that have changed since the last update
sub display_screen {
  my ($self) = @_;
  $self->{WIN}->move($self->{HEIGHT}, $self->{WIDTH});
  $self->{WIN}->refresh();
}

# create a single animation object
sub build_object {
  my ($self, %p) = @_;

  my %object = ();

  unless(defined($p{'name'})) {
    carp("No name supplied for object!");
    return \%object;
  }
  unless(defined($p{'shape'})) {
    carp("No shape supplied for object!");
    return \%object;
  }

  $object{NAME}         = $p{'name'};
  ($object{SHAPE}, $object{HEIGHT}, $object{WIDTH}) = $self->build_shape($p{'shape'});
  ($object{X}, $object{Y}, $object{Z})	= defined($p{'position'}) ? @{$p{'position'}} : [ 0, 0, 0 ];
  $object{CALLBACK_ARGS}= (defined($p{'callback_args'}))? $p{'callback_args'}: undef;
  $object{CURR_FRAME}   = (defined($p{'curr_frame'}))   ? $p{'curr_frame'}   : 0;
  $object{DEF_COLOR}	= (defined($p{'default_color'})) ? color_id($p{'default_color'}) : 'w';
  $object{WRAP}         = (defined($p{'wrap'}))         ? $p{'wrap'}         : 0;
  $object{TRANSPARENT}	= (defined($p{'transparent'}))  ? $p{'transparent'}  : '?';

  $object{AUTO_DEATH}	= (defined($p{'auto_death'}))   ? lc($p{'auto_death'}) : undef;
  $object{DEATH_ARG}	= (defined($p{'death_arg'}))    ? $p{'death_arg'}    : undef;
  $object{DEATH_CB}	= (defined($p{'death_cb'}))     ? $p{'death_cb'}     : undef;
  $object{DCB_ARGS}	= (defined($p{'dcb_args'}))     ? $p{'dcb_args'}     : undef;

  $object{EXTEND_X_POS} = (defined($p{'extend_x'}))     ? $p{'extend_x'} > 0 : 0;
  $object{EXTEND_X_NEG} = (defined($p{'extend_x'}))     ? $p{'extend_x'} < 0 : 0;
  $object{EXTEND_Y_POS} = (defined($p{'extend_y'}))     ? $p{'extend_y'} > 0 : 0;
  $object{EXTEND_Y_NEG} = (defined($p{'extend_y'}))     ? $p{'extend_y'} < 0 : 0;
  $object{EXTENDING}    = $object{EXTEND_X_POS} | $object{EXTEND_X_NEG} | $object{EXTEND_Y_POS} | $object{EXTEND_Y_NEG};

  $object{SUPPLIED_MASK} = $p{'color'};
  #($object{COLOR},$object{ATTR}) = $self->build_mask($object{DEF_COLOR}, $#{$object{SHAPE}}, $object{HEIGHT}, $object{WIDTH}, $p{'color'});
  ($object{COLOR},$object{ATTR}) = $self->build_mask(\%object);

  # $self->build_mask($object, $p{'color'});

  if   (defined($p{'callback'}))      { $object{CALLBACK} = $p{'callback'}; }
  elsif(defined($p{'callback_args'})) { $object{CALLBACK} = \&move_object;  }
  else                                { $object{CALLBACK} = undef;          }

  # a little bit of checking to make sure we built a valid object
  if(defined($object{AUTO_DEATH}) and $object{AUTO_DEATH} ne 'offscreen') {
    unless(defined($object{DEATH_ARG}) and $object{DEATH_ARG} =~ /^\d+$/) {
      croak("Auto death type $object{AUTO_DEATH} requires a positive integer argument");
    }
  }

  return \%object;
}

sub set_shape {
  my ($self, $object, $shape) = @_;
  unless(defined($self->{OBJECTS}{$object})) {
    carp("Attempt to update shape of nonexistant object $object");
    return $FAILURE;
  }
  ($self->{OBJECTS}{$object}{SHAPE}, $self->{OBJECTS}{$object}{HEIGHT}, $self->{OBJECTS}{$object}{WIDTH})
      = $self->build_shape($shape);
  return $SUCCESS;
}

sub set_color {
  my ($self, $object, $color) = @_;
  unless(defined($self->{OBJECTS}{$object})) {
    carp("Attempt to update color of nonexistant object $object");
    return $FAILURE;
  }
  $self->{OBJECTS}{$object}{SUPPLIED_MASK} = $color;
  ($self->{OBJECTS}{$object}{COLOR}, $self->{OBJECTS}{$object}{ATTR})
      = $self->build_mask($self->{OBJECTS}{$object});
  return $SUCCESS;
}

sub set_default_color {
  my ($self, $object, $color) = @_;
  unless(is_valid_color($color)) {
    carp("Invalid color supplied");
    return $FAILURE;
  }
  if(defined($self->{OBJECTS}{$object})) {
    $self->{OBJECTS}{$object}{DEF_COLOR} = color_id($color);
    ($self->{OBJECTS}{$object}{COLOR}, $self->{OBJECTS}{$object}{ATTR})
        = $self->build_mask($self->{OBJECTS}{$object});
    return $SUCCESS;
  }
  else {
    carp("Attempt to set color of nonexistant object '$object'");
    return $FAILURE;
  }  
}

# create a color mask for an object
sub build_mask {
  my ($self, $o) = @_;
  #my ($self, $def_color, $frames, $height, $width, $shape) = @_;

  # my $o = $self->{OBJECTS}{$object};
  my $def_color = $o->{DEF_COLOR};
  my $frames = $#{$o->{SHAPE}};
  my $shape = $o->{SUPPLIED_MASK};

  my @amask;
  my $mask = ();

  if(defined($shape)) {
    ($mask, undef, undef) = $self->build_shape($shape);
  }

  for my $f (0..$frames) {
    for my $i (0..$o->{HEIGHT}) {
      for my $j (0..$o->{WIDTH}) {
        if(!defined($mask->[$f][$i][$j]) or $mask->[$f][$i][$j] eq ' ') {
          $mask->[$f][$i][$j] = $def_color;
        }
        elsif(defined($mask->[$f][$i][$j])) {
          # make sure it's a valid color
          unless(defined($color_names->{$mask->[$f][$i][$j]})) {
            carp("Invalid color mask: $mask->[$f][$i][$j]");
            $mask->[$f][$i][$j] = undef;
          }
        }
        # capital letters indicate bold colors
        if($mask->[$f][$i][$j] =~ /[A-Z]/) {
          $mask->[$f][$i][$j] = lc($mask->[$f][$i][$j]);
          $amask[$f][$i][$j] = Curses::A_BOLD;
        }
      }
    }
  }
  return ($mask, \@amask);
}

# take one of 1) a string 2) an array of strings 3) an array of 2D arrays
# use these to generate a shape in the format we want (which is #3 above)
sub build_shape {
  my ($self, $shape) = @_;

  my @shape_array = ();
  my $height = 0;
  my $width = 0;

  if(ref($shape) eq 'ARRAY') {
    for my $i (0..$#{$shape}) {
      my $this_height = 0;
      if(ref($shape->[$i]) eq 'ARRAY') {
        $this_height = $#{$shape->[$i]};
        $shape_array[$i] = $shape->[$i];
      }
      else {
        # strip an empty line from the top, for convenience
        $shape->[$i] =~ s/^\n//;
        for my $line (split("\n", $shape->[$i])) {
          $this_height++;
          if(length($line) > $width) { $width = length($line); }
          push @{$shape_array[$i]}, [split('', $line)];
        }
      }
      if($this_height > $height) { $height = $this_height; }
    }
  }
  else {
    # strip an empty line from the top, for convenience
    $shape =~ s/^\n//;
    for my $line (split("\n", $shape)) {
      $height++;
      if(length($line) > $width) { $width = length($line); }
      push @{$shape_array[0]}, [split('', $line)];
    }
  }
  return \@shape_array, $height, $width;
}

# automatically make whitespace appearing on a line before the first non-
# whitespace character transparent
sub auto_trans {
  my ($self, $shape, $char) = @_;
  unless(defined($char)) { $char = '?'; }

  if(ref($shape) eq 'ARRAY') {
    my @shape_array = ();
    foreach my $i (0..$#{$shape}) {
      if(ref($shape->[$i] eq 'ARRAY')) {
        # unimplemented
      }
      else { push(@shape_array, trans_fill_string($self, $shape->[$i], $char)); }
    }
    return \@shape_array;
  }
  else {
    return trans_fill_string($self, $shape, $char);
  }

}

sub trans_fill_string {
  my ($self, $shape, $char) = @_;
  my $new = '';
  foreach my $line (split("\n", $shape)) {
    my $len = length(($line =~ /^(\s*)/)[0]);
    my $fill = ${char}x$len;
    $line =~ s/^\s{$len}/$fill/;
    $new .= $line . "\n";
  }
  return $new;
}

# simple callback to move and/or animate an object
sub move_object {
  my ($self, $object) = @_;
  my $o = $self->{OBJECTS}{$object};
  if(defined($o)) {
    my $cb_args;
    my $f;
    # figure out if we just have a set of deltas, or if we have
    # a full animation path to follow
    if(ref($o->{CALLBACK_ARGS}[1]) eq 'ARRAY') {
      $cb_args = $o->{CALLBACK_ARGS}[1][$o->{CALLBACK_ARGS}[0]];
      $o->{CALLBACK_ARGS}[0]++;
      if($o->{CALLBACK_ARGS}[0] > $#{$o->{CALLBACK_ARGS}[1]}) {
        $o->{CALLBACK_ARGS}[0] = 0;
      }
      $f = $cb_args->[3];
    }
    else {
      $cb_args = $o->{CALLBACK_ARGS};
      if($cb_args->[3]) {
        $f = $o->{CURR_FRAME} + $cb_args->[3];
        $f = ($f - int($f)) + ($f % ($#{$o->{SHAPE}} + 1));
      }
    }

    my $x = $o->{X} + $cb_args->[0];
    my $y = $o->{Y} + $cb_args->[1];
    my $z = $o->{Z} + $cb_args->[2];

    if($o->{WRAP}) {
      if($x > $self->{WIDTH})  { $x = ($x - int($x)) + ($x % $self->{WIDTH});  }
      elsif($x < 0)            { $x = ($x - int($x)) + ($x % $self->{WIDTH});  }
      if($y > $self->{HEIGHT}) { $y = ($y - int($y)) + ($y % $self->{HEIGHT}); }
      elsif($y < 0)            { $y = ($y - int($y)) + ($y % $self->{HEIGHT}); }
    }
    return($x, $y, $z, $f);
  }
  else {
    carp("Attempting to move nonexistant object '$object'");
  }
}

# given a start and end point, and a list of frames, return
# a list of deltas that are appropriate for the move_object routine
sub gen_path {
  my ($self, $x_start, $y_start, $z_start, $x_end, $y_end, $z_end, $frame_pattern, $steps_req) = @_;
  my @path = ();
  my $steps;

  my $x_dis = $x_end - $x_start;
  my $y_dis = $y_end - $y_start;
  my $z_dis = $z_end - $z_start;

  # default path length if none specified
  unless(defined($steps_req)) {
    $steps_req = 'shortest';
  }

  if($steps_req eq 'shortest' or $steps_req eq 'longest') {
    if($x_dis == $y_dis)  { $steps = $y_dis; }
    elsif($x_dis == 0)    { $steps = $y_dis; }
    elsif($y_dis == 0)    { $steps = $x_dis; }
    elsif(abs($x_dis) < abs($y_dis)) {
      if($steps_req eq 'shortest') { $steps = $x_dis; }
      else { $steps = $y_dis; }
    }
    else {
      if($steps_req eq 'shortest') { $steps = $y_dis; }
      else { $steps = $x_dis; }
    }
  }
  elsif($steps_req =~ /^\d+$/) { $steps = $steps_req; }
  elsif(uc($steps_req) eq 'X') { $steps = $x_dis; }
  elsif(uc($steps_req) eq 'Y') { $steps = $y_dis; }
  elsif(uc($steps_req) eq 'Z') { $steps = $z_dis; }
  else {
    carp("Unknown path length method: $steps_req"); return();
  }

  $steps = abs($steps);

  if($steps == 0) { carp("Cannot create a zero length path!"); return (); }
  elsif($steps == 1) {
    # a path length of one is a special case where we just move from the origin to the destination
    $path[0] = [($x_end - $x_start), ($y_end - $y_start), ($z_end - $z_start), $frame_pattern->[0]];
    return \@path;
  }

  my $x_incr = $x_dis / $steps;
  my $y_incr = $y_dis / $steps;
  my $z_incr = $z_dis / $steps;

  my ($x_pos, $y_pos, $z_pos) = ($x_start, $y_start, $z_start);
  my ($x_act, $y_act, $z_act) = ($x_start, $y_start, $z_start);

  for(0..$steps-2) {
    my ($x_prev, $y_prev, $z_prev) = ($x_pos, $y_pos, $z_pos);

    $x_pos+=$x_incr; $y_pos+=$y_incr; $z_pos+=$z_incr;
    
    my $f_pos = $frame_pattern->[${_}%($#{$frame_pattern}+1)];

    my ($x_mov, $y_mov, $z_mov) = (int($x_pos) - int($x_prev), int($y_pos) - int($y_prev), int($z_pos) - int($z_prev));
    $x_act += $x_mov; $y_act += $y_mov; $z_act += $z_mov;

    $path[$_] = [$x_mov, $y_mov, $z_mov, $f_pos];
  }

  # through rounding errors, we might end up with a final position that is off by one from
  # what we actually wanted. ending up in the right place is the most important thing,
  # so we just set the final position to put us where we want to be
  $path[$steps-1] = [$x_end - $x_act, $y_end - $y_act, $z_end - $z_act, $frame_pattern->[($steps - 1)%($#{$frame_pattern}+1)]];

  return \@path;
}

# run the callback routines for all objects that have them, and update
# the object accordingly. also checks for auto death status
sub do_callbacks {
  my ($self) = @_;

  my @kill_objects;
  my $time = time();

  foreach my $object (keys %{$self->{OBJECTS}}) {
    my $o = $self->{OBJECTS}{$object};

    if(defined($o->{AUTO_DEATH})) {
      if($o->{AUTO_DEATH} eq 'time' and $o->{DEATH_ARG} <= $time) {
        del_object($self, $object); next;
      }
      elsif($o->{AUTO_DEATH} eq 'frame' and ($o->{DEATH_ARG}--) <= 0) {
        del_object($self, $object); next;
      }
      elsif($o->{AUTO_DEATH} eq 'offscreen') {
        if($o->{X} > $self->{WIDTH} or $o->{Y} > $self->{HEIGHT} or
               $o->{X} < (0 - $o->{WIDTH}) or $o->{Y} < (0 - $o->{HEIGHT})) {
          push(@kill_objects, $object); next;
        }
      }
    }

    if(defined($o->{CALLBACK})) {
      my ($x, $y, $z, $f, $flag) = $o->{CALLBACK}->($self, $object);
      $o->{X} = defined($x) ? $x : $o->{X};
      $o->{Y} = defined($y) ? $y : $o->{Y};
      $o->{Z} = defined($z) ? $z : $o->{Z};
      $o->{CURR_FRAME} = defined($f) ? $f : $o->{CURR_FRAME};

      if(defined($flag)) {
        if($flag eq "kill") { del_object($self, $object); }
      }
    }
  }
  foreach my $object (@kill_objects) {
    del_object($self, $object);
  }
}

# stop curses cleanly
sub end {
  my ($self) = @_;
  endwin;
}

# write to a log file, for debugging
sub elog {
  my ($mesg) = @_;
  open(F, ">>", "elog.log");
  print F "$mesg\n";
  close(F);
}

1;
__END__

=head1 NAME

Term::Animation - ASCII sprite animation framework

=head1 SYNOPSIS

  use Term::Animation;

  # Constructors
  $screen = Term::Animation->new();
  $screen = Term::Animation->new($curses_window);

=head1 ABSTRACT

A framework to produce sprite animations using ASCII art.

=head1 DESCRIPTION

This module provides a framework to produce sprite animations using
ASCII art. Each ASCII 'sprite' is given one or more frames, and placed
into the animation as an 'animation object'. An animation object can
have a callback routine that controls the position and frame of the
object.

If the constructor is passed no arguments, it assumes that it is 
running full screen, and behaves accordingly. Alternatively, it can
accept a curses window (created with the Curses I<newwin> call) as an
argument, and will draw into that window.

=head1 EXAMPLES

This example moves a small object across the screen from left to right.

    use Term::Animation;
    use Curses;

    halfdelay( 2 );

    $screen = Term::Animation->new();

    # create a simple shape we can move around
    $shape = "<=O=>";

    # turn our shape into an animation object
    $object = $screen->build_object(
                 name          => "UFO",         # object name
                 shape         => $shape,        # object shape
                 position      => [3, 7, 10],    # row / column / depth
                 callback_args => [1, 0, 0, 0],  # the default callback
                                                 # routine takes a list
                                                 # of x,y,z,frame deltas
                 wrap          => 1              # screen wrap
             );

    # add the object to our animation
    $screen->add_object($object);

    # animation loop
    while(1) {
      # run the callback routines for all the objects, and update
      # the screen
      $screen->animate();

      # use getch to control the frame rate, and get input at the
      # same time. 
      my $input = getch();
      if($input eq 'q') { last; }
    }

    # cleanly end the animation, to avoid hosing up the user's terminal
    $screen->end();

This illustrates how to draw your animation into an existing Curses window.

    use Term::Animation;
    use Curses;

    # Term::Animation will not call initscr for you if
    # you pass it a window
    initscr();

    $win = newwin(5,10,8,7);

    $screen = Term::Animation->new($win);

Everything else would be identical to the previous example.

=head1 METHODS

=over 4

=item I<add_object ($object1, [ $object2 ...])>

Add one or more animation objects to the animation.

=item I<animate()>

Generate and display a single animation frame. Calls I<do_callbacks()>,
I<build_screen()> and I<display_screen()>. You can call them yourself
if you want to, but there is little useful you could do between these calls.

=item I<auto_trans ($shape, ['transparent character'])>

Given a sprite animation, this will return the animation with
transparency characters replacing any whitespace that appears on
a line before the first non-whitespace character. The default
transparent character is '?'. If you need to use '?' in your
ASCII art, you can pass an alternate character here, but you will
need to make sure you pass the same character as the C<transparent>
argument to the I<build_object()> routine.

=item I<build_object()>

Create an animation object given one or more frames of ASCII art,
and a set of arguments to describe the object's behavior. The only required
arguments are C<name> and C<shape>.

    name              A string uniquely identifying this object
    shape             The ASCII art for this object. It can be
                      provided as:
                      1) A single multi-line text string
                      2) An array of multi-line text strings
                      3) An array of 2D arrays, where each array
                         element is a single character
                      If you provide an array, each element is a
                      single frame of animation. If you provide
                      either 1) or 2), a single newline will be
                      stripped off of the beginning of each string.
    position          A list specifying initial x,y and z coordinates
                      Default: [ 0, 0, 0 ]
    callback          Callback routine for this object
                      Default: I<move_object()>
    callback_args     Arguments to the callback routine
    curr_frame        Animation frame to begin with
                      Default: 0
    wrap              Whether this object should wrap around the edge
                      of the screen (0 = No, 1 = Yes)
                      Default: 0
    transparent       Character used to indicate transparency
                      Default: ?
    auto_death        Method to automatically kill an object. Valid
                      values are 'offscreen', 'time', and 'frame'.
                      AUTO_DEATH section below for more detail.
    death_arg         Integer indicating 'time' or 'frame' for this
                      object to die
    death_cb          Callback routine used when this object dies
    dcb_args          Arguments to the death callback routine
    color             Color mask. This follows the same format as
                      'shape'. See the 'COLOR' section below for more
                      details
    default_color     A default color to use for the object.
                      See the 'COLOR' section below for more details

=item I<build_screen()>

Update the curses object in memory with any changes that have
been made after I<do_callbacks()> has run. After calling this, you
will need to call I<display_screen()> for the changes to show up on your
display.

=item I<color_enabled()>

Returns 1 if color is enabled, 0 otherwise

=item I<disable_color()>

Turn off ANSI color after it has been turned on using I<enable_color()>

=item I<display_screen()>

Update the display with the changes since the last update. Calling
this twice in a row without calling I<build_screen()> and
I<do_callbacks()> in the middle won't do anything.

=item I<do_callbacks()>

Run the callback routines for all of the objects in the animation.

=item I<enable_color()>

Turn on ANSI color. This MUST be called immediately after creating
the animation object, because the Curses start_color call must 
be made immediately. You can then turn color on and off using this
and I<disable_color()> whenever you want.

=item I<end()>

Run the Curses endwin function to get your terminal back to its
normal mode. You should call this before your program exits.

=item I<exist('object_name')>

Given an object name, will return true if it exists, false if it doesn't.

=item I<gen_path (x,y,z, x,y,z, [ frame_pattern ], [ steps ])>

Given beginning and end points, this will return a path for the
object to follow that can be given to the default callback routine,
I<move_object()>. The first set of x,y,z coordinates are the point
the object will begin at, the second set is the point the object
will end at. 

You can optionally supply a list of frames to cycle through. The list
will be repeated as many times as needed to finish the path.

You can also request the number of steps you would like for the object
to take to finish the path. Valid arguments are:
  longest      The longer of the X and Y distances
  shortest     The shorter of the X and Y distances
  X,Y or Z     Select the x, y or z distance
  <number>     Explicitly specify the number of steps to take

=item I<get_current_frame('object_name')>

Returns the current animation frame number of the named object. Carps
if the object does not exist.

=item I<get_position('object name')>

Returns the x,y,z coordinates of the named object. Carps if the object
does not exist.

=item I<move_object('object name')>

The default callback routine. Callback routines get their arguments from
the CALLBACK_ARGS element of the object they have been told to act on.
The data structure that move_object expects for CALLBACK_ARGS is either
a list of X,Y,Z and frame deltas or a path generated by I<gen_path()>.

=item I<redraw_screen()>

Clear everything from the screen, and redraw what should be there. This
should be called after I<update_term_size()>, or if the user indicates that
the screen should be redrawn to get rid of artifacts.

=item I<remove_all_objects()>

Remove every animation object. This is useful if you need to start the
animation over (eg. after a screen resize)

=item I<set_background('color_name')>

Change the background color. The default background color is black. You
can only have one background color for the entire Curses window that
the animation is running in.

=item I<size()>

Returns the number of characters in the curses window (width * height)

=item I<update_term_size()>

Call this if you suspect the terminal size has changed (eg. if you
get a SIGWINCH signal). Call I<remove_all_objects()> after this if
you want to recreate your animation from scratch.

=head1 CALLBACK ROUTINES

Callback routines for all objects are called each time I<do_callbacks()>
is called. A default callback routine is supplied, I<move_object()>, which
is sufficient for most basic movement. If you want to create an object
that exhibits more complex behavior, you will have to write a custom
callback routine for it.

Callback routines take a single argument, the name of the object to
act on. Any arguments required to tell the callback what to do with
the object, or any state that needs to be maintained, should be put
in the C<callback_args> element of the object. C<callback_args> is only
referenced by the callback routine, and thus can contain any datastructure
that you find useful.

The return value of your callback routine should be of the form:

    return ($x, $y, $z, $frame, $flag)

$x, $y and $z represent the X, Y and Z coordinates to which the object
should move. $frame is the frame number that the object should display,
if it has multiple frames of animation. $flag is a signal to 
I<do_callbacks()> to perform some action on this object. Currently,
the only valid value for $flag is 'kill', which will remove the object
from the animation, and call the objects death callback routine if there
is one. Any values that are unspecified or undef will remain unchanged.

=head1 AUTO_DEATH

Objects can be instructed to automatically die (remove themselves from
the animation) under certain circumstances, so that after they are
created they will clean up after themselves. There are three methods to
automatically kill an object:

    offscreen           The object is no longer visible on the screen
    time                The current time is later than a supplied time
    frame               A specified number of frames have been displayed 

The type of automatic death is specified as the C<auto_death>
argument to I<build_object()> when the object is created. the 'time'
and 'frame' auto death types require a value to be sent as the
C<death_arg> argument to build_object. For 'time', the argument
represents the time at which the the object should die, as returned
by I<localtime()> in scalar context. For 'frame', the argument is
the number of frames that should be displayed after this object is
added to the animation, before it dies. The 'offscreen' option does
not require a C<death_arg> argument.

=head1 COLOR

ANSI color is available for terminals that support it. Only a single
background color can be used for the window (it would look terrible
in most cases otherwise anyway). Colors for objects are specified by
using a 'mask' that indicates the color for each character. For
example, say we had a single frame of a bird:

$bird = q#

---. .-. .---
  --\'v'/--
     \ /
     " "
#;

To indicate the colors you want to use for the bird, create a matching
mask, with the first letter of each color in the appropriate position
(except black, which is 'k'). Pass this mask to I<build_object()> as
the 'color' parameter.

$mask = q#

BBBB BBB BBBB
  BBBWYWBBB
     B B
     Y Y
#;

When specifying a color, using uppercase indicates the color should be
bold. So 'BLUE' or 'B' means bold blue, and 'blue' or 'b' means non-bold
blue. 'Blue' means you get an error message.

You can also provide a default color with the default_color parameter
to build_object. This color will be used for any character that does
not have an entry in the mask. If you want the entire object to be
a single color, you can just provide a default color with no mask.

The available colors are: red, green, blue, cyan, magenta, yellow, black
and white.

Here's an example call to build_object for the bird above.

    $object = $screen->build_object (
                name		=> "Bird",
                shape		=> $bird,
                position	=> [ 5, 8, 7 ],
                callback_args	=> [ 1, 2, 0, 0 ],
                color		=> $mask,
                default_color	=> "BLUE"
                                    );

=head1 AUTHOR

Kirk Baucom, E<lt>kbaucom@schizoid.comE<gt>

=head1 SEE ALSO

L<Curses>

=cut
