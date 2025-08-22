/* dbgnew.h */
#ifndef DBGNEW_H
#define DBGNEW_H

#ifdef _DEBUG
    #define DBG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#else
    #define DBG_NEW new
#endif

#define new DBG_NEW

#endif // DBGNEW_H