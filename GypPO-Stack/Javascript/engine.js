Game = {
    start: function() {

        
    var x = $(window).width();
    var y = $(window).height();
    Crafty.init(x,y);

    Crafty.enterScene("Loading"); //enterScene will clear all entities on the screen
    }
}

//Leave for if things need to be loaded in before starting
Crafty.defineScene("Loading", function() {
    Environment.setScene("Loading");
    Crafty.background("#000");
    
    Crafty.e("2D, DOM, Text")
          .attr({ w: 100, h: 20, x: 150, y: 120 })
          .text("Loading")
          .textAlign("center")
          .textColor("#FFFFFF");

    Crafty.enterScene("Playable");
});

Crafty.defineScene("LvlWon", function() {
    Environment.setScene("Loading");
    Crafty.background("#000");
    
    Crafty.e("2D, DOM, Text")
          .attr({ w: 100, h: 20, x: 150, y: 120 })
          .text("Level Won!")
          .textAlign("center")
          .textColor("#FFFFFF");

    setTimeout(function(){ Crafty.enterScene("Playable") }, 500);
});

//Game over scene
Crafty.defineScene("GameOver", function() {
    Crafty.background("#000");
    
    Crafty.e("2D, DOM, Text")
          .attr({ w: 100, h: 20, x: 150, y: 120 })
          .text("Game Over!")
          .textAlign("center")
          .textColor("#FFFFFF");
});

Crafty.defineScene("GameWon", function () {
    Crafty.background ("#000");

    Crafty.e("2D, DOM, Text")
          .attr({ w: 100, h: 20, x: 150, y: 120 })
          .text("Game Won!")
          .textAlign("center")
          .textColor("#FFFFFF");
});

//Generate Level -> Win/Lose Level -> Enter Loading/Losing/Win scene (depending) -> Enter Playable scene (repeat until all levels are won)
Crafty.defineScene("Playable", function() {
    Environment.setScene("Playable");
    new Level(Levels.getCurrentLevel());
});

// Run on each frame of the game
// Used to move each entity in the game
Crafty.bind("UpdateFrame", function(Data) {
  var Move = Crafty("Motion").get();
  for (var x in Move) {   
    if (Move[x].hasOwnProperty("movement")) {
      Move[x].movement.pattern.move(Move[x], Move[x].movement.speed, Move[x].movement.dataSet);
    }
    if (Move[x].hasOwnProperty("triggerFunction")) { //Check if any entity has an event that needs to be triggered every frame (can be used for enemies firing weapons)
      Move[x].triggerFunction.function.apply(this, Move[x].triggerFunction.args); //have args as an array and use the apply function to pass them as function arguements
    }
  }
});


//Check if gravity on in case of flying enemies or other entities which are not affected by gravity
function createEntity(initPosition, shape, flying){
  var newEnt = new Crafty.e('2D, Motion, AngularMotion, Canvas, Color, Collision, Entity');
  newEnt.color(shape.colour);
  newEnt.attr({
            x: initPosition.x,
            y: initPosition.y,
            w: Environment.getCellHeight() * shape.width,
            h: Environment.getCellHeight() * shape.height
        });
  newEnt.facing = "left"; //Default direction the entity is facing when spawned
  newEnt.checkHits('Entity','Pickup');
  newEnt.bind("HitOn", function (hitData) {
          dispatchFunction(this, hitData);
        });
  if (!flying) {
    newEnt.addComponent('Gravity');
    newEnt.gravity('Floor');
  }
  return newEnt;
}

//Create collectible item (no hitbox for this entity)
function createPickup(initPosition, colour){
  return new Crafty.e('2D, Canvas, Color')
        .color(colour)
        .attr({ 
            x: initPosition.x + (Environment.getCellWidth() /2), //Place in the center of a grid unit
            y: initPosition.y - (Environment.getCellHeight() /2),
            w: 10,
            h: 10
        });
}

//Create a generic floor
//provide x, y coordinates on map as well as size of platform
//Not exclusive to just platforms
function Floor(coords, size, clr){
    return new Crafty.e('Floor, 2D, Color, Collision, Canvas')
       .color(clr)
       .attr({
            x: coords.x,
            y: coords.y,
            w: size*(Environment.getCellWidth()),
            h: 5
        });
}

//function to create text on screen
function createText(x, y, text) {
    return new Crafty.e("2D, Canvas, Text")
        .attr({x: x, y: y})
        .textColor('white')
        .text(text);
}

// Create invisible entity which is used just for its collision detection
function createHitbox(coords, h, w) {
  var yoffset = coords.y - h;
  return new Crafty.e("Entity, 2D, Canvas")
    .attr({x : coords.x, y: yoffset, h: h, w: w});
} 

function shortestDistance(from, toEntitySet) {
  var closestEntity = null;
  var minDist = Number.MAX_SAFE_INTEGER;
  for (var i in toEntitySet) {
    if (toEntitySet[i] != from) {
      var distance = Crafty.math.distance(from.x, from.y, toEntitySet[i].x, toEntitySet[i].y);
      if (distance < minDist) {
        closestEntity = toEntitySet[i];
        minDist = distance;
      }
    }
  }
  return closestEntity;
}

function entityJumping(entity, speed) {
  entity.addComponent('Jumper');
  entity.jumpSpeed(speed);
  entity.jump();
}

//Create a free scrolling camera which centers around a single game entity
//Requires entity to center on, as well as grid region height and width (based on grid units)
function freeCamera(focusEnt, h, w) {
  var followEnt = Crafty(focusEnt).get(0);
  var CameraCenter = Crafty.e('2D, DOM').attr({x: followEnt.x, y: followEnt.y}); //Make an invisible entity which is always at the center of the viewport
  
  var trueH = h * Environment.getCellHeight();
  var trueW = w * Environment.getCellWidth();
  Crafty.viewport.clampToEntities = false;
  Crafty.viewport.width = trueW;
  Crafty.viewport.height = trueH;
  Crafty.viewport.follow(followEnt,0,0); //Center camera on the entity and follow it

  //Attach any HUD elements to the center of the viewport so that they will always be visible as viewport moves
  //Need to adjust text's position so that they are at the top right position of the viewport
  var Text = Crafty('Text').get();
  for (var i in Text) {
    // Text[i].y = Text[i].y - trueH/2 + 10;
    // Text[i].x = Text[i].x - trueW/2 + 10;
    Text[i].x -= Crafty.viewport.x;
    Text[i].y -= Crafty.viewport.y;
    CameraCenter.attach(Text[i]);
  }

  //When the entity the viewport is following is destroyed, needs to get the newest entity to follow
  Crafty.bind("CameraAnimationDone", function() {
    followEnt = Crafty(focusEnt).get(0);
    Crafty.viewport.follow(followEnt, 0, 0);
  });

  //Move the invisible camera entity as the viewport is moving
  Crafty.bind("ViewportScroll", function() {
    CameraCenter.attr({x: followEnt.x, y: followEnt.y});
  });
}

//Create a one-way scrolling camera which centers around a single game entity
//Requires entity to center on, as well as grid region height and width (based on grid units)
//Will center on the entity when they are moving rightwards but stop moving if the entity is moving backwards
function oneWayCamera(focusEnt, h, w) {
  var followEnt = Crafty(focusEnt).get(0);
  Crafty.viewport.x = followEnt.x;
  Crafty.viewport.y = followEnt.y;
  
  var trueH = h * Environment.getCellHeight();
  var trueW = w * Environment.getCellWidth();
  Crafty.viewport.clampToEntities = false;
  Crafty.viewport.width = trueW;
  Crafty.viewport.height = trueH;

  //When the focus entity comes into contact with it, move along with the entity, but do not move back if the focus entity does
  var CameraCenterSide = Crafty.e('2D, DOM, Motion, Collision') //Make an invisible entity which is always at the center of the viewport
    .attr({x: followEnt.x+10, y: followEnt.y - trueH, h: Environment.getWindowDimensions().h, w: 1})
    .checkHits(focusEnt)
    .bind("HitOn", function(hitDatas) {
      for (var i in hitDatas) {
        this.vx = hitDatas[i].obj.vx;
        CameraCenter.vx = this.vx;
      }
    })
    .bind("HitOff", function(d) {
      this.vx = 0;
      CameraCenter.vy = 0;
      this.x = followEnt.x + 10;
      this.y = followEnt.y - trueH;
      CameraCenter.attr({x: CameraCenterSide.x, y: followEnt.y});
    });
    
  var CameraCenter = Crafty.e('2D, DOM, Motion').attr({x: CameraCenterSide.x, y: followEnt.y});
  followEnt.attach(CameraCenter);
  Crafty.viewport.follow(CameraCenter,0,0);

  //Create an edge boundary preventing the focus entity from leaving the viewport
  var edge = Crafty.e('2D, DOM, Collision')
    .attr({x: CameraCenterSide.x - trueW/2, y: CameraCenterSide.y, h: Environment.getWindowDimensions().h, w:1})
    .checkHits(focusEnt)
    .bind("HitOn", function(hitDatas) {
      for (var i in hitDatas) {
        hitDatas[i].obj.vx = 0;
        hitDatas[i].obj.x = (hitDatas[i].obj.x < this.x) ? this.x : hitDatas[i].obj.x+1; //Prevents the focus entity from going below the x position of this edge
      }
      this.resetHitChecks(focusEnt);
    });

    CameraCenterSide.attach(edge);
  //Attach any HUD elements to the center of the viewport so that they will always be visible as viewport moves
  //Need to adjust text's position so that they are at the top right position of the viewport
  var Text = Crafty('Text').get();
  for (var i in Text) {
    Text[i].x -= Crafty.viewport.x;
    Text[i].y -= Crafty.viewport.y;
    CameraCenter.attach(Text[i]);
  }
  //When the focus entity is destroyed, needs to get the newest entity to follow
  Crafty.bind("CameraAnimationDone", function() {
    followEnt = Crafty(focusEnt).get(0);
    CameraCenterSide.x = followEnt.x+10;
    CameraCenterSide.y = followEnt.y - trueH;
    CameraCenter.attr({x: CameraCenterSide.x, y: followEnt.y});
    followEnt.attach(CameraCenter);
  });

  Crafty.bind("ViewportScroll", function() {
    CameraCenter.attr({x: CameraCenterSide.x, y: followEnt.y});
  });
}

var Environment = {

  init: function() {
    var self = this;
    self.height = $(window).height();
    self.width = $(window).width();
    self.cols = [];
    self.rows = [];
    self.coords = {x: {}, y: {}};
    self.score = 0;
    self.start = false;
    self.cellW = 0;
    self.cellH = 0;
    self.scene = "Loading";
  },

  grid: function(x, y) {
    var self = this,
      wm = Math.round(self.width / x),
      hm = Math.round(self.height / y);
    for (var i = 0; i <= x; i++) {
      var realX = wm * i;
      self.coords.x[i] = realX;
    }

    for (var i = 0; i <= y; i++) {
      var realY = hm * i;
      self.coords.y[i] = realY;
    }

    self.cellW = self.coords.x[1] - self.coords.x[0];
    self.cellH = self.coords.y[1] - self.coords.y[0];
  },

  begin: function(start) {
    var self = this;
    self.start = start;
    Game.start();
  },

  getRealCoords: function (coords) {
    var self = this;
    return {
      x: self.coords.x[coords[0]],
      y: self.coords.y[coords[1]]
    };
  },

  getCellWidth: function() {
    var self = this;
    return self.cellW;
  },

  getCellHeight: function() {
    var self = this;
    return self.cellH;
  },

  getWindowDimensions: function() {
    var self = this;
    return {
      w: self.width,
      h: self.height
    };
  },

  setScene: function(scene) {
    var self = this;
    self.scene = scene;
  },

  getScene: function() {
    var self = this;
    return self.scene;
  },

  HUDDisplay: function(elements) {
    var self = this;
    self.HUDelements = elements;
    self.CraftyText = {}
    for (i in elements) {
      var elemFunc = elements[i].function;
     self.CraftyText[i] = new Crafty.e("2D, Canvas, Text")
        .attr({x: (75*i), y: 0})
        .textColor('white')
        .text(elements[i].txt + ": " + elemFunc());
    }
  },

  HUDUpdate: function() {
    var self = this;
    for (i in self.CraftyText) {
      var elemFunc = self.HUDelements[i].function;
      self.CraftyText[i].text(self.HUDelements[i].txt + ": " + elemFunc());
    }
  }

}

var Levels = {
  init: function(lvls) {
    var self = this;
    self.levels = lvls;
    self.current = 0;
  },

  getCurrentLevel: function() {
    var self = this;
    return self.levels[self.current];
  },

  nextLevel: function() {
    var self = this;
    if (self.current < self.levels.length - 1) {
      self.current += 1;
      Crafty.enterScene('LvlWon');  
    } else {
      Crafty.enterScene('GameWon');
    }
  }
}