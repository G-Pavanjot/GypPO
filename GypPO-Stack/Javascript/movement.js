function MovementPattern() {
  var self = this;
  self.start = 0;
}
MovementPattern.prototype.move = function(entity, speed, x) {};

// Although entity remains stationary, should have consistent functions so that the code generator does not have to generate unique code for different patterns
function Still() {
  MovementPattern.call(this);
}
Still.prototype = new MovementPattern();
Still.prototype.constructor = Still();
Still.prototype.move = function(entity, speed,x) {
  checkTracking(entity, entity.movement.targets);
};

// Move entity between two points
// Requires the crafty entity, speed and the two points to move between
// Points = {p1: (x1, y1), p2: (x2, y2)}
function Patrol() {
  MovementPattern.call(this);
}
Patrol.prototype = new MovementPattern();
Patrol.prototype.constructor = Patrol();
Patrol.prototype.move = function(entity, speed, points) {
  if (entity.movement.v) {
      entityJumping(entity, speed);
  }
  if(entity.moving == "left"){
    if(entity.x <= points.start.x) {
      entity.vx = speed;
      entity.moving = "right";
    } else{
      entity.vx = (-1 * speed);
    }
  }else{
    if(entity.x >= points.end.x) {
      entity.vx = (-1 * speed);
      entity.moving = "left"; //Moving and facing need to be separated in case the entity has tracking (face the direction of tracked entity)
    } else {
      entity.vx = speed;
    }
  }

  checkFacing(entity);
  checkTracking(entity, entity.movement.targets);
};

function Charge() {
  MovementPattern.call(this);
}
Charge.prototype = new MovementPattern();
Charge.prototype.constructor = Charge();
//Move towards the target entity
Charge.prototype.move = function(entity, speed,target) {
  if (entity.movement.v) {
      entityJumping(entity, speed);
  }
  if (entity.x < target.x) {
    entity.vx = speed;
  } else {
    entity.vx = (-1 * speed);
  }
  if (entity.y < target.y) {
    entity.vy = speed;
  } else {
    entity.vy = (-1 * speed);
  }

  checkFacing(entity);
  checkTracking(entity, entity.movement.targets);
};

function Random() {
  MovementPattern.call(this);
  var self = this;
  self.frames = 0;
}
Random.prototype = new MovementPattern();
Random.prototype.constructor = Random();

// If flying entity: 4 Directions the entity can randomly move in + becoming stationary
// If ground entity: 2 directions + becoming stationary + jumping
// So the entity holds a direction for a number of frames, limit the occurence of the random change to once per 30 frames
Random.prototype.move = function(entity, speed, x) {
  var self = this;
  var randNum = Math.floor(Math.random() * Math.floor(100));
  self.frames += 1;
  if (self.frames % 30 == 0) {
  if (entity.has('Gravity')) {
  switch (randNum % 4) {
    case 0: //Go right
      entity.vx = speed;
      break;
    case 1: //Go left
      entity.vx = (-1 * speed);
      break;
    case 2: //Jump
      entityJumping
      break;
    case 3: //Stationary
      entity.vx = 0;
      break;
  }
  } else {
    switch (randNum % 5) {
    case 0: //Go right
      entity.vx = speed;
      break;
    case 1: //Go left
      entity.vx = (-1 * speed);
      break;
    case 2: //Stationary
      entity.vx = 0;
      entity.vy = 0;
      break;
    case 3:
      entity.vy = speed;
      break;
    case 4:
      entity.vy = (-1 * speed);
      break;
    }
  }
}
  checkFacing(entity);
  checkTracking(entity, entity.movement.targets);
};


// ---------- PROJECTILE MOVEMENT BELOW -----------------------------
// Fire projectile straight ahead until it reaches its range (or is destroyed by colliding into the protag or antag)
function Straight() {
  MovementPattern.call(this);
}
Straight.prototype = new MovementPattern();
Straight.prototype.constructor = Straight();
Straight.prototype.move = function(entity, speed,range) {
  if (Crafty.math.distance(entity.start.x, entity.start.y, entity.x,entity.y) < range){
    entity.vx = speed;
  }else{
    entity.destroy();
  }
};

function Arc() {
  MovementPattern.call(this);
}
Arc.prototype = new MovementPattern();
Arc.prototype.constructor = Arc();
Arc.prototype.move = function(entity, speed, range) {
  var rotationOrigin = (speed < 0) ? (-1 * range/2) : range/2;
  entity.origin(rotationOrigin, 0);
  if (Math.abs(entity.rotation) < 180) {
    entity.vrotation = speed;
  }else {
    entity.destroy();
  }
};

function VAtk() {
  MovementPattern.call(this);
}
VAtk.prototype = new MovementPattern();
VAtk.prototype.constructor = VAtk();
VAtk.prototype.move = function(entity, speed,range) {
  entity.onHit('Floor', function(hitData) {
    entity.vy = (-1 * Math.abs(speed));
  });
  if (Crafty.math.distance(entity.start.x, entity.start.y, entity.x,entity.y) < range) {
    entity.vx = speed;
    if (entity.y <= entity.start.y + entity.whoFired.h/2) {
      entity.vy = Math.abs(speed);
    }
  } else {
     entity.destroy();
  }
};

function Homing() {
  MovementPattern.call(this);
}
Homing.prototype = new MovementPattern();
Homing.prototype.constructor = Homing();
Homing.prototype.move = function(entity, speed, data) {
  var absSpeed = Math.abs(speed);
  if (data.target != null) {
  if ((Crafty.math.distance(entity.start.x, entity.start.y, entity.x, entity.y) < data.range) && (data.target != null)){

    //Round to closest whole pixel position
    if (Math.round(entity.x) < Math.round(data.target.x)){
      entity.vx = absSpeed;
    }else if(Math.round(entity.x) > Math.round(data.target.x)){
      entity.vx = (-1 * absSpeed);
    } else if (Math.round(entity.x) == Math.round(data.target.x)) {
      entity.vx = 0;
    }
      
    if (Math.round(entity.y) < Math.round(data.target.y)){
      entity.vy = absSpeed
    } else if(Math.round(entity.y) > Math.round(data.target.y)){
      entity.vy = (-1 * absSpeed)
    } else if (Math.round(entity.y) == Math.round(data.target.y)) {
      entity.vy = 0;
    }
    
  } else {
    entity.destroy();
  }
  if ((Math.abs(entity.x - data.target.x) < 3) && (Math.abs(entity.y - data.target.y) < 3)) {
    entity.destroy(); //object reached the target location but it's still within the range (within 3 pixels otherwise the positions are too precise)
  }
} else {
  entity.destroy();
}
};

function Swing() {
  MovementPattern.call(this);
}
Swing.prototype = new MovementPattern();
Swing.prototype.constructor = Swing();
Swing.prototype.move = function(entity, speed, data) {
  if (Math.abs(entity.rotation) < 165) { //Need to check absolute rotation angle as negate speed (swinging to the left) gives negative angles
    entity.vrotation = speed;
  }else {
    entity.destroy();
  }
};

function checkFacing(entity) {
  if (entity.vx > 0) {
    entity.facing = "right";
  } else if (entity.vx < 0){
    entity.facing = "left";
  }
}

function checkTracking(entity, target) {
  if (entity.movement.track) {
    var trackThis = Crafty(target).get(0);
    if (trackThis.x < entity.x) {
      entity.facing = "left";
    } else {
      entity.facing = "right";
    }
  }
}