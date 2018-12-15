########## Simulation main program ##########

###### Part 1: Define several functions ######

#### special minimum function ####
min_special<-function(x){
  if(sum(x)>0){
    result<-min(x[x>0])
  }
  else{
    result<-0
  }
  return(result)
}

#### function for spinning process ####
# N: number of spinning machines
# Q: queue length
# processing time ~ N(240,120)
# status: 0 for idle, 1 for busy
spin<-function(spin_machine){
  N<-length(spin_machine[,1])
  avail<-N-sum(spin_machine[,1])
  Q<-spin_machine[1,5]
  
  if(Q>0 & avail>0){
  if(avail<=Q){
  j=1
  for(i in 1:N){
    if(spin_machine[i,1]==0 & j<=avail){
      spin_machine[i,1]=1
      spin_machine[i,2]<-rnorm(1,240,sqrt(120))
      j=j+1
    }
  }
  spin_machine[,5]<-spin_machine[,5]-avail 
  }
  else{
  j=1
    for(i in 1:N){
      if(spin_machine[i,1]==0 & j<=Q){
        spin_machine[i,1]=1
        spin_machine[i,2]<-rnorm(1,240,sqrt(120))
        j=j+1
      }
    }
  spin_machine[,5]<-spin_machine[,5]-Q
  }
  t_process<-min_special(spin_machine[,2])
  k<-match(t_process,spin_machine[,2])
  spin_machine_finish<-spin_machine
  spin_machine_finish[,6]<-rep(1,N)*t_process
  spin_machine_finish[,7]<-rep(k,N)
  }
  
  else{
    t_process<-min_special(spin_machine[,2])
    k<-match(t_process,spin_machine[,2])
    spin_machine_finish<-spin_machine
    spin_machine_finish[,6]<-rep(1,N)*t_process
    spin_machine_finish[,7]<-rep(k,N)
    }
  
  return(spin_machine_finish)
  }

#### function for weaving process ####
# N: number of weaving machines
# Q: queue length
# processing time ~ N(480,200)
# status: 0 for idle, 1 for busy
weave<-function(weave_machine){
  N<-length(weave_machine[,1])
  avail<-N-sum(weave_machine[,1])
  Q<-weave_machine[1,5]
  
  if(Q>0 & avail>0){
  if(avail<=Q){
  j=1
    for(i in 1:N){
      if(weave_machine[i,1]==0 & j<=avail){
        weave_machine[i,1]=1
        weave_machine[i,2]<-rnorm(1,480,sqrt(200))
        j=j+1
      }
    }
    weave_machine[,5]<-weave_machine[,5]-avail
  }
  else{
  j=1
    for(i in 1:N){
      if(weave_machine[i,1]==0 & j<=Q){
        weave_machine[i,1]=1
        weave_machine[i,2]<-rnorm(1,480,sqrt(200))
        j=j+1
      }
    }
    weave_machine[,5]<-weave_machine[,5]-Q
  }
  t_process<-min_special(weave_machine[,2])
  k<-match(t_process,weave_machine[,2])
  weave_machine_finish<-weave_machine
  weave_machine_finish[,6]<-rep(1,N)*t_process
  weave_machine_finish[,7]<-rep(k,N)
  }
  else{
    t_process<-min_special(weave_machine[,2])
    k<-match(t_process,weave_machine[,2])
    weave_machine_finish<-weave_machine
    weave_machine_finish[,6]<-rep(1,N)*t_process
    weave_machine_finish[,7]<-rep(k,N)
  }
  return(weave_machine_finish)
}

#### function for finishing process ####
# N: number of finishing machines
# Q: queue length
# processing time ~ exp(120)
# status: 0 for idle, 1 for busy
finish<-function(finish_machine){
  N<-length(finish_machine[,1])
  avail<-N-sum(finish_machine[,1])
  Q<-finish_machine[1,5]
  
  if(Q>0 & avail>0){
  j=1
  if(avail<=Q){
    for(i in 1:N){
      if(finish_machine[i,1]==0 & j<=avail){
        finish_machine[i,1]=1
        finish_machine[i,2]<-rexp(1,rate=1/120)
        j=j+1
      }
    }
    finish_machine[,5]<-finish_machine[,5]-avail
  }
  else{
  j=1
    for(i in 1:N){
      if(finish_machine[i,1]==0 & j<=Q){
        finish_machine[i,1]=1
        finish_machine[i,2]<-rexp(1,rate=1/120)
        j=j+1
      }
    }
    finish_machine[,5]<-finish_machine[,5]-Q
  }
  t_process<-min_special(finish_machine[,2])
  k<-match(t_process,finish_machine[,2])
  finish_machine_finish<-finish_machine
  finish_machine_finish[,6]<-rep(1,N)*t_process
  finish_machine_finish[,7]<-rep(k,N)
  }
  else{
    t_process<-min_special(finish_machine[,2])
    k<-match(t_process,finish_machine[,2])
    finish_machine_finish<-finish_machine
    finish_machine_finish[,6]<-rep(1,N)*t_process
    finish_machine_finish[,7]<-rep(k,N)
  }
  
  return(finish_machine_finish)
}

#### function for packing process ####
# N: number of packing machines
# Q: queue length
# processing time ~ exp(360)
# status: 0 for idle, 1 for busy
pack<-function(pack_machine){
  N<-length(pack_machine[,1])
  avail<-N-sum(pack_machine[,1])
  Q<-pack_machine[1,5]
  
  if(Q>0 & avail>0){
  j=1
  if(avail<=Q){
    for(i in 1:N){
      if(pack_machine[i,1]==0 & j<=avail){
        pack_machine[i,1]=1
        pack_machine[i,2]<-rexp(1,rate=1/360)
        j=j+1
      }
    }
    pack_machine[,5]<-pack_machine[,5]-avail
  }
  else{
  j=1
    for(i in 1:N){
      if(pack_machine[i,1]==0 & j<=Q){
        pack_machine[i,1]=1
        pack_machine[i,2]<-rexp(1,rate=1/360)
        j=j+1
      }
    }
    pack_machine[,5]<-pack_machine[,5]-Q
  }
    t_process<-min_special(pack_machine[,2])
    k<-match(t_process,pack_machine[,2])
    pack_machine_finish<-pack_machine
    pack_machine_finish[,6]<-rep(1,N)*t_process
    pack_machine_finish[,7]<-rep(k,N)
  }
  else{
    t_process<-min_special(pack_machine[,2])
    k<-match(t_process,pack_machine[,2])
    pack_machine_finish<-pack_machine
    pack_machine_finish[,6]<-rep(1,N)*t_process
    pack_machine_finish[,7]<-rep(k,N)
  }
  return(pack_machine_finish)
}

#### function to handle connection between different stages ####
pass<-function(spin_machine,weave_machine,finish_machine,pack_machine){
  # check all process first
  spin_machine_check<-spin(spin_machine)
  weave_machine_check<-weave(weave_machine)
  finish_machine_check<-finish(finish_machine)
  pack_machine_check<-pack(pack_machine)
  # find the earliest finish time
  process<-c(spin_machine_check[1,6],weave_machine_check[1,6],finish_machine_check[1,6],pack_machine_check[1,6])
  real_process_time<-min_special(process)
  #find the earliest finish process
  real_process<-match(real_process_time,process)
  if(real_process==1){
    # release one machine in spin process
    spin_machine_out<-spin_machine_check
    spin_machine_out[,2]<-spin_machine_check[,2]-real_process_time*spin_machine_check[,1]
    spin_machine_out[,3]<-spin_machine_check[,3]+real_process_time*spin_machine_check[,1]
    spin_machine_out[,4]<-spin_machine_check[,4]+real_process_time
    spin_machine_out[spin_machine_out[1,7],1]<-0
    # add 1 to queue of weave process
    weave_machine_out<-weave_machine_check
    weave_machine_out[,2]<-weave_machine_check[,2]-real_process_time*weave_machine_check[,1]
    weave_machine_out[,3]<-weave_machine_check[,3]+real_process_time*weave_machine_check[,1]
    weave_machine_out[,4]<-weave_machine_check[,4]+real_process_time
    weave_machine_out[,5]<-weave_machine_check[,5]+1
    # let the time fly for finish process
    finish_machine_out<-finish_machine_check
    finish_machine_out[,2]<-finish_machine_check[,2]-real_process_time*finish_machine_check[,1]
    finish_machine_out[,3]<-finish_machine_check[,3]+real_process_time*finish_machine_check[,1]
    finish_machine_out[,4]<-finish_machine_check[,4]+real_process_time
    # let time fly for pack process
    pack_machine_out<-pack_machine_check
    pack_machine_out[,2]<-pack_machine_check[,2]-real_process_time*pack_machine_check[,1]
    pack_machine_out[,3]<-pack_machine_check[,3]+real_process_time*pack_machine_check[,1]
    pack_machine_out[,4]<-pack_machine_check[,4]+real_process_time
  }
  if(real_process==2){
    # let time fly for spin process
    spin_machine_out<-spin_machine_check
    spin_machine_out[,2]<-spin_machine_check[,2]-real_process_time*spin_machine_check[,1]
    spin_machine_out[,3]<-spin_machine_check[,3]+real_process_time*spin_machine_check[,1]
    spin_machine_out[,4]<-spin_machine_check[,4]+real_process_time
    # release one machine in weave process
    weave_machine_out<-weave_machine_check
    weave_machine_out[,2]<-weave_machine_check[,2]-real_process_time*weave_machine_check[,1]
    weave_machine_out[,3]<-weave_machine_check[,3]+real_process_time*weave_machine_check[,1]
    weave_machine_out[,4]<-weave_machine_check[,4]+real_process_time
    weave_machine_out[weave_machine_out[1,7],1]<-0
    # add 1 to quene of finish process
    finish_machine_out<-finish_machine_check
    finish_machine_out[,2]<-finish_machine_check[,2]-real_process_time*finish_machine_check[,1]
    finish_machine_out[,3]<-finish_machine_check[,3]+real_process_time*finish_machine_check[,1]
    finish_machine_out[,4]<-finish_machine_check[,4]+real_process_time
    finish_machine_out[,5]<-finish_machine_check[,5]+1
    # let time fly for pack process
    pack_machine_out<-pack_machine_check
    pack_machine_out[,2]<-pack_machine_check[,2]-real_process_time*pack_machine_check[,1]
    pack_machine_out[,3]<-pack_machine_check[,3]+real_process_time*pack_machine_check[,1]
    pack_machine_out[,4]<-pack_machine_check[,4]+real_process_time
  }
  if(real_process==3){
    # let time fly for spin process
    spin_machine_out<-spin_machine_check
    spin_machine_out[,2]<-spin_machine_check[,2]-real_process_time*spin_machine_check[,1]
    spin_machine_out[,3]<-spin_machine_check[,3]+real_process_time*spin_machine_check[,1]
    spin_machine_out[,4]<-spin_machine_check[,4]+real_process_time
    # let time fly for weave process
    weave_machine_out<-weave_machine_check
    weave_machine_out[,2]<-weave_machine_check[,2]-real_process_time*weave_machine_check[,1]
    weave_machine_out[,3]<-weave_machine_check[,3]+real_process_time*weave_machine_check[,1]
    weave_machine_out[,4]<-weave_machine_check[,4]+real_process_time
    # release 1 machine for finish process
    finish_machine_out<-finish_machine_check
    finish_machine_out[,2]<-finish_machine_check[,2]-real_process_time*finish_machine_check[,1]
    finish_machine_out[,3]<-finish_machine_check[,3]+real_process_time*finish_machine_check[,1]
    finish_machine_out[,4]<-finish_machine_check[,4]+real_process_time
    finish_machine_out[finish_machine_out[1,7],1]<-0
    # add 1 to queue of pack process
    pack_machine_out<-pack_machine_check
    pack_machine_out[,2]<-pack_machine_check[,2]-real_process_time*pack_machine_check[,1]
    pack_machine_out[,3]<-pack_machine_check[,3]+real_process_time*pack_machine_check[,1]
    pack_machine_out[,4]<-pack_machine_check[,4]+real_process_time
    pack_machine_out[,5]<-pack_machine_check[,5]+1
  }
  if(real_process==4){
    # add 1 to the queue of spin process
    spin_machine_out<-spin_machine_check
    spin_machine_out[,2]<-spin_machine_check[,2]-real_process_time*spin_machine_check[,1]
    spin_machine_out[,3]<-spin_machine_check[,3]+real_process_time*spin_machine_check[,1]
    spin_machine_out[,4]<-spin_machine_check[,4]+real_process_time
    spin_machine_out[,5]<-spin_machine_check[,5]+1
    # let time fly for weave process
    weave_machine_out<-weave_machine_check
    weave_machine_out[,2]<-weave_machine_check[,2]-real_process_time*weave_machine_check[,1]
    weave_machine_out[,3]<-weave_machine_check[,3]+real_process_time*weave_machine_check[,1]
    weave_machine_out[,4]<-weave_machine_check[,4]+real_process_time
    # let time fly for finish process
    finish_machine_out<-finish_machine_check
    finish_machine_out[,2]<-finish_machine_check[,2]-real_process_time*finish_machine_check[,1]
    finish_machine_out[,3]<-finish_machine_check[,3]+real_process_time*finish_machine_check[,1]
    finish_machine_out[,4]<-finish_machine_check[,4]+real_process_time
    # release 1 machine for pack process
    pack_machine_out<-pack_machine_check
    pack_machine_out[,2]<-pack_machine_check[,2]-real_process_time*pack_machine_check[,1]
    pack_machine_out[,3]<-pack_machine_check[,3]+real_process_time*pack_machine_check[,1]
    pack_machine_out[,4]<-pack_machine_check[,4]+real_process_time
    pack_machine_out[pack_machine_out[1,7],1]<-0
  }
  result<-list(spin_machine_out,weave_machine_out,finish_machine_out,pack_machine_out)
  return(result)
}

#### Main simulation function ####
simulation<-function(N_A,N_B,N_C,N_D,Q_0=25){
  spin_machine_begin<-matrix(0,N_A,7)
  weave_machine_begin<-matrix(0,N_B,7)
  finish_machine_begin<-matrix(0,N_C,7)
  pack_machine_begin<-matrix(0,N_D,7)
  spin_machine_begin[,5]<-Q_0*rep(1,N_A)
  while (t_clock<=t_end) {
    status<-pass(spin_machine_begin,weave_machine_begin,finish_machine_begin,pack_machine_begin)
    spin_machine_begin<-status[[1]]
    weave_machine_begin<-status[[2]]
    finish_machine_begin<-status[[3]]
    pack_machine_begin<-status[[4]]
    t_clock<-status[[1]][1,4]
  }
  occupy_rate<-sum(sum(status[[1]][,3]),sum(status[[2]][,3]),sum(status[[3]][,3]),sum(status[[4]][,3]))/sum(sum(status[[1]][,4]),sum(status[[2]][,4]),sum(status[[3]][,4]),sum(status[[4]][,4]))
  idle_rate<-1-occupy_rate
  result<-c(N_A,N_B,N_C,N_D,idle_rate)
  return(result)
}


###### Part 2: Simulation start ######

#### Initialization ####
t_clock<-0 #clock time
t_end=7*24*3600 #end time
Q_0=25 #initial queue length
result_table<-data.frame("Spinning"=0,"Weaving"=0,"Finishing"=0,"Packing"=0,"Avg_idle_rate"=0) #table to store results

#### Loop all combinations to find optimal solution ####
for (a in 1:10){
  for (b in 1:10){
    for (c in 1:10){
      for (d in 1:10){
        result_table<-rbind(result_table,simulation(a,b,c,d))
      }
    }
  }
}
result_table<-result_table[-c(1),]

#### Sort result table ####

#### Export result table to csv file ####
write.csv(result_table,file="results.csv",row.names=FALSE)