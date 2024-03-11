#include <Message_ProgressRange.hxx>
#include "hs_Message_ProgressRange.h"

Message_ProgressRange * hs_new_Message_ProgressRange(){
    return new Message_ProgressRange();
}

void hs_delete_Message_ProgressRange(Message_ProgressRange * theRange){
    delete theRange;
}