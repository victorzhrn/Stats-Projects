library(plot3D)
library(ggplot2)

# gravitational acceleration
g = -9.81
max_mass = 4/3*pi*9^3

# water drop over delta 
drop_p <- function(p,delta_t,x_v,y_v,z_a=-981){
  p_radius = p[7]
  p_mass = 4/3*pi*p_radius^3   # unit gram
  p_area = pi*p_radius^2
  #surface_mass_ratio <- p_area/p_mass
  coef = delta_t/0.01
  x_v = x_v*coef
  y_v = y_v*coef
  z_a = z_a*coef
  local_vx = p[4]
  new_x = p[1] + local_vx*delta_t
  local_vy = p[5]
  new_y = p[2]  + local_vy*delta_t
  local_vz = p[6]
  new_z = p[3] + 0.5*(delta_t^2)*z_a + local_vz*delta_t
  
  # drag coefficient
  k = 0.02
  x_a = k*(local_vx^2-x_v^2)
  y_a = k*(local_vy^2-y_v^2)
  
  new_vx = local_vx+delta_t*x_a
  new_vy = local_vy+delta_t*y_a
  new_vz = delta_t*z_a + local_vz
  
  new_p = c(new_x,new_y,new_z,new_vx,new_vy,new_vz,p[7],p[8]+delta_t)
  return(new_p)
}

split_drop<- function(p,variation=5){
  p1 = p
  p2 = p
  p1[4] = p1[4]+rnorm(1,0,variation)
  p1[5] = p1[5]+rnorm(1,0,variation)
  p2[4] = p2[4]+rnorm(1,0,variation)
  p2[5] = p2[5]+rnorm(1,0,variation)
  return(list(p1,p2))
}

drop_till_ground<-function(p,delta_t,x_v,y_v,g=-981){
  df_local_p = data.frame()
  df_local_p <- rbind(df_local_p,p)
  names(df_local_p) <- c("x","y","z",'v_x','v_y','v_z','r','t')
  p_local_current = p
  split_condition = FALSE
  while(p_local_current[3]>0 & !split_condition){
    new_p = drop_p(p_local_current,delta_t,x_v,y_v,g)
    df_local_p = rbind(df_local_p,new_p)
    if(new_p[7]>0.2 & sample(c(0,1),1,prob=c(0.95,0.05))==1){
      new_p[7] = new_p[7]*(2)^(-1/3)
      p_split_1 = unlist(split_drop(new_p)[1])
      p_split_2 = unlist(split_drop(new_p)[2])
      df_local_p = rbind(df_local_p,drop_till_ground(p_split_1,delta_t,x_v,y_v))
      df_local_p = rbind(df_local_p,drop_till_ground(p_split_2,delta_t,x_v,y_v))
      split_condition=TRUE
    }
    p_local_current = new_p
    
  }
  return(df_local_p)
}

# components of a points
# df_p: 1:x,2:y,3:z,4:v_x,5:v_y,6:v_z,7:r,8:t

generate_bigDrops <- function(x=0,y=0,z=500){
  d = rnorm(1,0,30)
  angle = runif(1,0,2*pi)
  x = x+cos(angle)*d
  y = y + sin(angle)*d
  return(c(x,y,z,0,0,0,1.5,0))
}

get_bound <- function(df_p,alpha = 0.95){
  df_p_ground <- df_p[df_p$z<=0,]
  df_p_ground['distance'] = sqrt(df_p_ground$x^2+df_p_ground$y^2)
  return(quantile(df_p_ground$distance,alpha))
}

# define gamma distribution
n=20
shape = 2
scale= 4/sqrt(pi)
rate = 1/scale
wind <- rgamma(n,shape,scale)
wind <- wind*10

main_simulate <- function(n=3,wind){
  df_p <- data.frame()
  for(i in 1:n){
    p_test = generate_bigDrops(z=1000)
    angle = runif(1,0,2*pi)
    print(wind[i]/sin(angle))
    df_test <- drop_till_ground(p_test,delta_t=0.01,x_v=wind[i]*cos(angle),y_v=wind[i]/sin(angle))
    df_p <- rbind(df_p,df_test)
    names(df_p) <- c("x","y","z",'v_x','v_y','v_z','r','t')
  }
  return(df_p)
}

df_p = main_simulate(n,wind)
scatter3D(x=df_p$x,y=df_p$y,z=df_p$z,cex=df_p$r,theta =45,phi=45,xlim=c(-300,100))

# calculate fountain radius needed
get_bound(df_p)

df_p_hit = df_p[df_p$z<=0,]
scatter2D(x=df_p_hit$x,y=df_p_hit$y,ylim=c(-300,300),xlim=c(-200,200))

#ggplot(df_p_hit,aes(x,y))+geom_point()





