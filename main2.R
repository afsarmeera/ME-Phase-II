#1<-charging.2<-discharging.3<-powergeneration.
#library(random)
z<-900
#

for (i in 1:110) {
  for(j in 1:110) 
  {
    act=a$a[j]
    
    
    if(act==1)
    {
      d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
      d$x[i]<-sqrt(d$x[i]*d$x[i])
      if(i==1)
      {
        d$x[i]<-d$x[i]
      }
      else
      {
        d$x[i]<-d$x[i]+d$x[i-1]
      }
      if (d$x[i] < z||d$x[i]>0)
      {
        reward<-1
        a$d[i]=cons$x[i]
        a$p[i]=ren$x[i]+nr$x[i]
        a$a[i]=act
        a$r[i]=reward
        a$b[i]=d$x[i]
      }
      else
      {
        reward<-0
        a$d[i]=cons$x[i]
        a$p[i]=ren$x[i]+nr$x[i]
        a$a[i]=act
        a$r[i]=reward
        a$b[i]=d$x[i]
      }
      
    }
    
    if(act==3)
      
    {
      
      
      if(cons$x[i]>(ren$x[i]+nr$x[i])&&d$x[i]>0||d$x[i]>z)
        reward<-1
      a$d[i]=cons$x[i]
      a$p[i]=ren$x[i]+nr$x[i]
      a$a[i]=act
      a$r[i]=reward
      d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
      d$x[i]<-sqrt(d$x[i]*d$x[i])
      if(i==1)
      {
        d$x[i]<--(d$x[i])
      }
      else
      {
        d$x[i]<-(d$x[i])+d$x[i-1]
      }
      a$b[i]=d$x[i]
    }
    
    else {
      reward<-0
      a$d[i]=cons$x[i]
      a$p[i]=ren$x[i]+nr$x[i]
      a$a[i]=act
      a$r[i]=reward
      d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
      d$x[i]<-sqrt(d$x[i]*d$x[i])
      if(i==1)
      {
        d$x[i]<--(d$x[i])
      }
      else
      {
        d$x[i]<-(-d$x[i])+d$x[i-1]
      }
      a$b[i]=d$x[i]
    }
    
    if(act==2)
    {
      if(d$x[i]<0&&cons$x[i]>ren$x[i]+nr$x[i]+d$x[i])
      {
        reward<-1
        a$d[i]=cons$x[i]
        a$p[i]=ren$x[i]+nr$x[i]
        a$a[i]=act
        a$r[i]=reward
        d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
        d$x[i]<-sqrt(d$x[i]*d$x[i])
        if(i==1)
        {
          d$x[i]<-d$x[i]
        }
        else
        {
          d$x[i]<-d$x[i-1]
        }
        a$b[i]=d$x[i]
      }
      else
      {
        
        reward<-0
        a$d[i]=cons$x[i]
        a$p[i]=ren$x[i]+nr$x[i]
        a$a[i]=act
        a$r[i]=reward
        d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
        d$x[i]<-sqrt(d$x[i]*d$x[i])
        if(i==1)
        {
          d$x[i]<-d$x[i]
        }
        else
        {
          d$x[i]<-d$x[i-1]
        }
        a$b[i]=d$x[i]
        
        
      }
      
      
    }
    
    if(act>3||act==0)
    {
      for(j in y <- round(runif(1, min=1, max=3))) 
      {
        act=j
        
        if(act==1)
        {
          d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
          d$x[i]<-sqrt(d$x[i]*d$x[i])
          if(i==1)
          {
            d$x[i]<-d$x[i]
          }
          else
          {
            d$x[i]<-d$x[i]+d$x[i-1]
          }
          if (d$x[i] < z||d$x[i]>0)
          {
            reward<-1
            a$d[i]=cons$x[i]
            a$p[i]=ren$x[i]+nr$x[i]
            a$a[i]=act
            a$r[i]=reward
            a$b[i]=d$x[i]
          }
          else
          {
            reward<-0
            a$d[i]=cons$x[i]
            a$p[i]=ren$x[i]+nr$x[i]
            a$a[i]=act
            a$r[i]=reward
            a$b[i]=d$x[i]
          }
          
        }
        
        if(act==3)
          
        {
          
          
          if(cons$x[i]>(ren$x[i]+nr$x[i])&&d$x[i]>0||d$x[i]>z)
            reward<-1
          a$d[i]=cons$x[i]
          a$p[i]=ren$x[i]+nr$x[i]
          a$a[i]=act
          a$r[i]=reward
          d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
          d$x[i]<-sqrt(d$x[i]*d$x[i])
          if(i==1)
          {
            d$x[i]<--(d$x[i])
          }
          else
          {
            d$x[i]<-(d$x[i])+d$x[i-1]
          }
          a$b[i]=d$x[i]
        }
        
        else {
          reward<-0
          a$d[i]=cons$x[i]
          a$p[i]=ren$x[i]+nr$x[i]
          a$a[i]=act
          a$r[i]=reward
          d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
          d$x[i]<-sqrt(d$x[i]*d$x[i])
          if(i==1)
          {
            d$x[i]<--(d$x[i])
          }
          else
          {
            d$x[i]<-(-d$x[i])+d$x[i-1]
          }
          a$b[i]=d$x[i]
        }
        
        if(act==2)
        {
          if(d$x[i]<0&&cons$x[i]>ren$x[i]+nr$x[i]+d$x[i])
          {
            reward<-1
            a$d[i]=cons$x[i]
            a$p[i]=ren$x[i]+nr$x[i]
            a$a[i]=act
            a$r[i]=reward
            d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
            d$x[i]<-sqrt(d$x[i]*d$x[i])
            if(i==1)
            {
              d$x[i]<-d$x[i]
            }
            else
            {
              d$x[i]<-d$x[i-1]
            }
            a$b[i]=d$x[i]
          }
          else
          {
            
            reward<-0
            a$d[i]=cons$x[i]
            a$p[i]=ren$x[i]+nr$x[i]
            a$a[i]=act
            a$r[i]=reward
            d$x[i]<- cons$x[i]-(ren$x[i]+nr$x[i])
            d$x[i]<-sqrt(d$x[i]*d$x[i])
            if(i==1)
            {
              d$x[i]<-d$x[i]
            }
            else
            {
              d$x[i]<-d$x[i-1]
            }
            a$b[i]=d$x[i]
            
            
          }
          
          
        }
       
        
      }
      
    }
    
  }
  
}


