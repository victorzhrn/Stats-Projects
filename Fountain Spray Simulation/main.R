library(plot3D)

# gravitational acceleration
g = -9.81
max_mass = 4/3*pi*9^3

# water drop over delta 
drop_p <- function(p,delta_t,x_a,y_a,z_a=-9.81){
  p_radius = p[8]
  p_mass = 4/3*pi*p_radius^3   # unit gram
  p_area = pi*p_radius^2
  #surface_mass_ratio <- p_area/p_mass
  
  local_vx = p[4]
  new_x = p[1] + 0.5*delta_t^2*x_a + local_vx*delta_t
  local_vy = p[5]
  new_y = p[2] + 0.5*delta_t^2*y_a + local_vy*delta_t
  local_vz = p[6]
  new_z = p[3] + 0.5*delta_t^2*z_a + local_vz*delta_t
  
  new_vx = delta_t*x_a + local_vx
  new_vy = delta_t*y_a + local_vy
  new_vz = delta_t*z_a + local_vz
  new_p = c(new_x,new_y,new_z,new_vx,new_vy,new_vz,p[7],p[8]+delta_t)
  return(new_p)
}

split_drop<- function(p,delta_t){
  p1 = p
  p2 = p
 
  p1[1] = p1[1] + p1[4]+rnorm(1,0,p1[4]*delta_t/4)
  p1[2] = p1[2] + p1[5]+rnorm(1,0,p1[5]*delta_t/4)
  p2[1] = p2[1] + p2[4]+rnorm(1,0,p2[4]*delta_t/4)
  p2[2] = p2[2] + p2[5]+rnorm(1,0,p2[5]*delta_t/4)
  return(list(p1,p2))
}

drop_till_ground<-function(p,delta_t,x_a,y_a,g=-9.81){
  df_local_p = data.frame()
  df_local_p <- rbind(df_local_p,p)
  names(df_local_p) <- c("x","y","z",'v_x','v_y','v_z','r','t')
  p_local_current = p
  while(p_local_current[3]>0){
    new_p = drop_p(p_local_current,delta_t,x_a,y_a,g)
    df_local_p = rbind(df_local_p,new_p)
    if(new_p[7]>0.2 & sample(c(0,1),1,prob=c(0.95,0.05))==1){
      new_p[7] = new_p[7]/2
      df_local_p = rbind(df_local_p,drop_till_ground(new_p,delta_t,x_a,y_a))
      new_p[1]= new_p[1]+1
    }
    p_local_current = new_p
    
  }
  return(df_local_p)
}


# components of a points
# df_p: 1:x,2:y,3:z,4:v_x,5:v_y,6:v_z,7:r,8:t
df_p <- data.frame()

p_test =  c(1,1,10,-0.8,-0.6,0,2,0)
df_test <- drop_till_ground(p_test,0.01,0.8,-1.2)

df_p <- rbind(df_p,df_test)
names(df_p) <- c("x","y","z",'v_x','v_y','v_z','r','t')


scatter3D(x=df_p$x,y=df_p$y,z=df_p$z,cex=df_p$r,theta =45,phi=0)




