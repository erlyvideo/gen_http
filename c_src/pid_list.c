#include "gen_http.h"

void pid_list_send(PidList *a, ErlDrvPort port, ErlDrvTermData *reply, size_t reply_size) {
  PidList *next;
  
  
  while(a) {
    next = a->next;
    driver_send_term(port, a->pid, reply, reply_size);
    a = next;
  }  
}

void pid_list_free(PidList **head) {
  PidList *a = *head;
  PidList *next;
  
  
  while(a) {
    next = a->next;
    driver_free(a);
    a = next;
  }
  
  *head = NULL;
}

int pid_list_add_caller(PidList **head, ErlDrvPort port) {
  PidList *acceptor = driver_alloc(sizeof(PidList));
  acceptor->next = *head;
  acceptor->pid = driver_caller(port);
  
  if (driver_monitor_process(port, acceptor->pid ,&acceptor->monitor) != 0) {
    driver_free(acceptor);
    return 0;
  }
  
  *head = acceptor;
  return 1;
}


void pid_list_delete(PidList **head, ErlDrvPort port, ErlDrvMonitor *monitor) {
  
  ErlDrvTermData pid = driver_get_monitored_process(port, monitor);
  
  PidList *a, *next;
  PidList *root = NULL;
  PidList *deleting = NULL;
  
  a = *head;
  
  while(a) {
    next = a->next;
    if(a->pid == pid) {
      a->next = deleting;
      deleting = a;
    } else {
      a->next = root;
      root = a;
    }
    a = next;
  }
  *head = root;
  while(deleting) {
    // fprintf(stderr, "Removing dead acceptor\r\n");
    
    
    driver_demonitor_process(port, &deleting->monitor);
    next = deleting->next;
    driver_free(deleting);
    deleting = next;
  }
  
}


void pid_list_remove_head(PidList **head, ErlDrvPort port) {
  if(!*head) return;
  driver_demonitor_process(port, &(*head)->monitor);
  PidList *old = *head;
  *head = (*head)->next;
  driver_free(old);
}
