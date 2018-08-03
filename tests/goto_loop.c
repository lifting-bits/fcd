int a = 0;
int b = 0;
int main(void) {
    c1:
        if (a == 0)
            goto n1;
        else
            goto c2;
    
    n1:
        b += 1;
        goto c1;
    
    c2:
        if (a == 1)
            goto n2;
        else
            goto n3;
    
    n2:
        b += 2;
        goto n9;
    
    n3:
        b += 3;
        goto c3;
    
    c3:
        if (a == 2)
            goto c1;
        else 
            goto n9;
    
    n9:
        return 0;
}