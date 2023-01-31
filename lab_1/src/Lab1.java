import TSim.*;

import java.io.PipedOutputStream;
import java.util.ArrayList;
import java.util.concurrent.*;

import javax.lang.model.util.ElementScanner14;
import javax.swing.text.Position;

public class Lab1 {
  ArrayList<Critical> sections = new ArrayList(); 
  TSimInterface tsi = TSimInterface.getInstance();


  public Lab1(int speed1, int speed2) {
    Critical up_right = new Critical();
    up_right.add(11,23);
    
    // create threads for each train
    Thread t_1 = new Thread(new Train(1,speed1));
    t_1.start();
    Thread t_2 = new Thread(new Train(2,speed2));
    t_2.start();
    

    try {
      tsi.setSpeed(1,speed1);
      tsi.setSpeed(2,speed2);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }


  }

  private Critical isCritical(int x, int y){
    for (Critical c : sections) {
      for (Lab1.Critical.Sensor p : c.positions) {
        if(p.x == x && p.y == y){
          return c;
        }
      }
      
    }
    return null;
  }

  public void tryTrain(Critical c, Train t){
    if(c.tryAcquire()) {
      //if(sensor.isUp){switch.left or switch.right}

      return;
    }
    else {
      //find sensor
      //if()
      return;
    }
}

public void setSwitch(int x, int y, int dir) {
    try{
      tsi.setSwitch(x,y, dir);
    }

    catch (CommandException e){
      e.printStackTrace();
      System.exit(1);

    } 
}

  
  private class Critical extends Semaphore{
      private ArrayList<Sensor> positions = new ArrayList();  
      private static int set_id = 0;
      private final int id;

      private class Sensor {
        int x;
        int y;
        
        public Sensor(int x ,int y) {
          this.x = x;
          this.y = y;
        }
      }

      public Critical(){
        super(1,true);
        this.id = this.set_id;
        this.set_id++;

        
      }

      public void add(int x, int y){
        this.positions.add(new Sensor(x,y));
      }

      public void remove(int i){
        this.positions.remove(i);
      }

      public int indexOf(Sensor sensor) {
        return positions.indexOf(sensor);
      }
    }

  private class Train implements Runnable{
    
    private final int id;
    private final int speed;
    private SensorEvent sensor;
    private Critical current;

    public Train(int id, int speed){
      this.id = id;
      this.speed = speed;
    }

    void setSpeed(int speed){
        try {
          tsi.setSpeed(this.id, speed);
        }

        catch (CommandException e){
          e.printStackTrace();
          System.exit(1);

        }
    }
    int getIndex(Critical c) {
      for (Lab1.Critical.Sensor sensor : c.positions) {
        if(sensor.x == this.sensor.getXpos() && sensor.y == this.sensor.getYpos())
          return c.indexOf(sensor);
          break;
      }
      return -1;
    }

    void check_track(Critical c, Train t){
      
      int index = getIndex(c);
      //TODO make exception
      
      t.setSpeed(0);
      if(c.tryAcquire()){
        switch(c.id) {
          case(0): t.changeSection(c); section_zero(t,index); break; 
          }
      }

      t.setSpeed(t.speed);
    }

    public void changeSection(Critical c) {
      try {
        c.acquire(); this.current.release(); this.current = c;
      }
      catch (InterruptedException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }
  
    public void section_zero (Train t, int sensor){
      int switch1_x = 0;
      int switch1_y = 0;
      int switch2_x = 0;
      int switch2_y = 0;
      switch(sensor){
        case(0):
        case(2):
          if(true /* !t.isAcquired */) {
            if(sections.get(1).tryAcquire())
            setSwitch(switch1_x, switch1_y,tsi.SWITCH_LEFT);
            else 
            setSwitch(switch1_x, switch1_y, tsi.SWITCH_RIGHT);
            //t.isAcquired == true;
          }
          else {
            //t.isAcquired == false;
            //release Critical
          }
          return; 
        case(1):
        case(3):
          if(true /* !t.isAcquired */)
            if(sections.get(3).tryAcquire())
              setSwitch(switch2_x, switch2_y, tsi.SWITCH_RIGHT);
            else
              setSwitch(switch2_x, switch2_y,tsi.SWITCH_LEFT);
          //t.isAcquired == true;
              else {
            //t.isAcquired == false;
            //release Critical
          }
          return;
      }
    }
    public void run(){
      while(true){
        try {
          this.sensor = tsi.getSensor(this.id);
          Critical current = isCritical(sensor.getXpos(), sensor.getYpos());
          check_track(current, this);
        } 

        catch (CommandException e){
          e.printStackTrace();
          System.exit(1);

        }

        catch (InterruptedException e) {
          e.printStackTrace();
          System.exit(1);
        }


        }
      }
  }  
}
