def memoize(procedure):
    memos_list = {}
    def proc(x):
        if x in memos_list: 
            return memos_list[x]
        else:
            t = procedure(x, proc)
            memos_list[x] = t 
            return memos_list[x]
    return proc


            

        
