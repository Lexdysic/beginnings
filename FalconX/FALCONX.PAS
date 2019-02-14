program FalconX (Input, Output);

uses crt,dos,graph,svga;

const leng=100; density=600; enmys=30; limit=30; astrod=50; bgcolor=1; fgcolor=9;
const move : array[1..57] of integer =
(0,7,14,20,26,31,36,40,44,47,50,52,54,55,56,56,54,52,50,47,44,40,36,31,26,20,14,7,0,-7,-14,-20,-26,-31,-36,-40,-44,-47,-50,-52,
-54,-55,-56,-56,-55,-54,-52,-50,-47,-44,-40,-36,-31,-26,-20,-14,-7);

type astros   = array[1..astrod] of record x,y,h,s,c :integer; io:boolean;end;
     shoot    = array[1..limit] of record x,y,p :integer; s:boolean; end;
     enmy     = array[1..enmys] of record x,y,h,l :integer;end;
     star     = array[1..density] of record x,y,c:integer;end;
     powerup  = record x,y:integer; t:string; end;
     fire_arr = array[1..5] of integer;
     stat     = array[1..7] of longint;

var s,j,i,u,show :integer;
    st        :string;
    pos       :PointType;
    stars     :star;
    fire,nfire:fire_arr;
    ast       :astros;
    stats     :stat;
    shot      :shoot;
    what      :array[1..4] of string;
    nmy       :enmy;
    clock:longint;
    Up,Down,Left,Right,Space,Esc,Enter,F1,F2,F3,F4,exit : Boolean;
    BIOSHandler : procedure;

{$F+}
procedure KeyCheck; interrupt;
var CallBIOSHandler :Boolean;
begin
     CallBIOSHandler := true;
     case port [$60] of
          72 : begin
             Up := true;
             CallBIOSHandler := false;
          end;
          200 : begin
              Up := false;
              CallBIOSHandler := false;
          end;
          75 : begin
             Left := true;
             CallBIOSHandler := false;
          end;
          203 : begin
              Left := false;
              CallBIOSHandler := false;
          end;
          77 : begin
             Right := true;
             CallBIOSHandler := false;
          end;
          205 : begin
              Right := false;
              CallBIOSHandler := false;
          end;
          80 : begin
             Down := true;
             CallBIOSHandler := false;
          end;
          208 : begin
              Down := false;
              CallBIOSHandler := false;
          end;
          57 : begin
             Space := true;
             CallBIOSHandler := false;
          end;
          185 : begin
              space := false;
              CallBIOSHandler := false;
          end;
          28 : begin
             Enter := true;
             CallBIOSHandler := false;
          end;
          156 : begin
              Enter := false;
              CallBIOSHandler := false;
          end;
          59 : begin
             F1 := true;
             CallBIOSHandler := false;
          end;
          187 : begin
              F1 := false;
              CallBIOSHandler := false;
          end;
          60 : begin
             F2 := true;
             CallBIOSHandler := false;
          end;
          188 : begin
              F2 := false;
              CallBIOSHandler := false;
          end;
          61 : begin
             F3 := true;
             CallBIOSHandler := false;
          end;
          189 : begin
              F3 := false;
              CallBIOSHandler := false;
          end;
          62 : begin
             F4 := true;
             CallBIOSHandler := false;
          end;
          190 : begin
              F4 := false;
              CallBIOSHandler := false;
          end;
          1 : begin
            if esc=true then esc:=false else esc:=true;
          end;
     end;
     if CallBIOSHandler then begin
        asm pushf end;
        BIOSHandler;
     end else begin
         port[$20] := $20;
     end;
end; {$F-}

procedure XKey;
begin
     GetIntVec (9, @BIOSHandler);
     SetIntVec (9, Addr(KeyCheck));
end;

procedure NKey;
begin
     SetIntVec (9, @BIOSHandler);
end;

procedure wait (MicroSecs : LongInt);
assembler;
asm
   mov cx, word ptr [MicroSecs+2]
   mov dx, word ptr [MicroSecs]
   mov ah, $86
   int $15
end;

Procedure save;
Var  F   :Text;
     i   :integer;
     s,g :string;
Begin
     Assign (f, 'saveinfo.sav');
     Rewrite(f);
     for i:=1 to 6 do
     begin
          str(stats[i],g);
          writeln(f,g);
     end;
     str(clock,g);
     writeln(f ,g);
     setcolor(lightblue);
     outtextxy(290,300,'Saved');
     Close (f);
End;
Procedure load;
Var  F :Text;
     t,i,code :integer;
     s :string;
Begin
     Assign (f, 'saveinfo.sav');
     reset(f);
     for i:=1 to 6 do
     begin
          Readln(f,s);
          val(s,t,code);
          stats[i]:=t;
     end;
     Readln(f,s);
     val(s,t,code);
     stats[i]:=t;
     setcolor(lightblue);
     outtextxy(290,300,'Loaded');
     Close (f);
End;

Procedure logo;
var ch   :char;
    i,w  :integer;
begin
     settextstyle(6,0,10);
     setcolor(39);
     for i:=0 to 5 do
     outtextxy(100+i, 150+i,'Falcon X');
     readkey;
end;

procedure init;
var i,count,counts,dots,f,spot,rand :integer;
    p,per                           :real;
begin
     dots:=4;
     textcolor(bgcolor);
     write('Loading Data');
     for i:= 1 to leng do
     begin
          rand:=random(100);
          if rand = 0 then i:=i-1;
          if rand = 99 then i:=i+1;
          textcolor(bgcolor);
          gotoxy(1,2);
          p := (i/leng)*100;
          write(p:0:0,'%');
          gotoxy(80,25);
          if (i mod 3 = 0) and (i mod 2 = 0) then
          begin
               gotoxy(13,1);
               clrEol;
               case dots of
                    4 : begin dots:=0;end;
                    3 : begin write('...');dots:=4;end;
                    2 : begin write('..'); dots:=3;end;
                    1 : begin write('.');  dots:=2;end;
                    0 : dots:=1;
               end;
               gotoxy(80,25);
          End;
          spot := round(26 * (i/leng));
          gotoxy(5,2);
          textcolor(fgcolor);
          for count:=1 to spot do
              write(chr(219));
          gotoxy(80,25);
          sound(200+random(20));
          wait(20);
          sound(1000+random(50));
          wait(30+random(10));
          nosound;
     end;
end;

procedure mission;
var str1,str2,str3 :string;
    i,s            :integer;
    ch             :char;
begin
     clrscr;
     textcolor(bgcolor);
     write(chr(201));
     for i:=1 to 78 do
         write(chr(205));
     write(chr(187));
     gotoxy(3,1); write('Message');
     str1:='You have new orders; The Federation needs you to to the the melark system';
     str2:='to clear astroids in the return path of the convoy. We need your help to';
     str3:='get them through. We have trust in you, May the florce be with you.';
     for s:=2 to 5 do
     begin
          gotoxy(1,s);write(chr(186));
          gotoxy(80,s);write(chr(186));
     end;
     gotoxy(1,s);
     write(chr(200));
     for i:=1 to 78 do
         write(chr(205));
     write(chr(188));
     gotoxy(65,s);write('Press a button');
     textcolor(fgcolor);
     gotoxy(2,2);
     for i:=1 to length(str1) do
     begin
         write(str1[i]);
         wait(20);
     end;
     gotoxy(2,3);
     for i:=1 to length(str2) do
     begin
         write(str2[i]);
         wait(20);
     end;
     gotoxy(2,4);
     for i:=1 to length(str3) do
     begin
         write(str3[i]);
         wait(20);
     end;
     readkey;
end;

procedure hit(x,y,w:integer);
var i,th,s :integer;
begin
     setcolor(lightblue);
     setfillstyle(1,white);
     fillellipse(x,y,w,w div 2);
     fillellipse(x,y,w div 2,w);
     s:=200+random(100);
     sound(s);
     wait(5);
     nosound;
end;

procedure statistics;
var ww,wp,s  :integer;
    p,q      :string;
    FillInfo :FillSettingsType;
begin
     setcolor(30);
     rectangle(539,469,640,480);{power}
     line(560,458,640,458);
     ellipse(559,469,90,180,20,10);
     rectangle(460,469,539,480);{points}
     ellipse(480,469,90,180,20,10);
     line(480,458,560,458);
     rectangle(370,469,460,480);{weapons}
     ellipse(390,469,90,180,20,10);
     line(390,458,480,458);
     rectangle(300,469,370,480);{level}
     ellipse(320,469,90,180,20,10);
     line(320,458,390,458);
     setcolor(54);  {text}
     settextstyle(2,0,4);
     outtextxy(577,457,'Power');
     outtextxy(485,457,'Points');
     outtextxy(395,457,'Weapons');
     outtextxy(323,457,'Level');
     str(stats[3],p);
     outtextxy(500-(textwidth(p) div 2),470,p);
     str(stats[2],p);
     str(stats[4],q);
     p:= q+ ' of ' +p;
     outtextxy(415-(textwidth(p) div 2),470,p);
     str(stats[5],p);
     outtextxy(335-(textwidth(p) div 2),470,p);
     {rectangle}
     wp:=640-stats[1];
     ww:=640-stats[7];
     setfillstyle(1,80);
     bar(wp,475,639,479);
     setfillstyle(1,175);
     bar(ww,474,639,470);
     str(clock,p);
     outtextxy(10,470,p);
     str(fire[5],p);
     outtextxy(50,470,p);
end;

procedure drawship;
var x,y:integer;
begin
     x:=pos.x;y:=pos.y;
     setcolor(7);
     setfillstyle(1,7);
     arc(x,y-15,180,360,20);
     arc(x-10,y-15,180,360,9);
     arc(x+10,y-15,180,360,9);
     bar(x-1,y-20,x+1,y);
     circle(x,y-18,2);
     circle(x,y-20,3);
     line(x,y-20,x,y-18);
     floodfill(x,y+1,7);
     setfillstyle(1,8);
     floodfill(x,y-4,7);
     setfillstyle(1,8);
     floodfill(x-19,y-11,7);
     floodfill(x+19,y-11,7);
end;

procedure drawastro;
var i,rand :integer;
begin
     for i:=1 to astrod do
     begin
          if ast[i].io=true then
          begin
               ast[i].y:=ast[i].y+2+random(2);
               if random(2) = 0 then ast[i].X:=ast[i].x-random(2)
               else ast[i].X:=ast[i].x+random(2);
               if (ast[i].y>500) and (clock<600)then
               begin
                    ast[i].y:=0-100-random(150);
                    ast[i].x:=random(640);
                    ast[i].s:=8+random(3);
                    ast[i].h:=100;
                    ast[i].c:=random(5)+20;
               end;
               if (ast[i].y>500) and (clock>=600)then ast[i].io:=false;
               setcolor(ast[i].c);
               setfillstyle(1,ast[i].c);
               if (ast[i].h>50) then
               begin
                    circle(ast[i].x, ast[i].y, ast[i].s);
                    floodfill(ast[i].x, ast[i].y, ast[i].c);
                    floodfill(ast[i].x-(ast[i].s)+1, ast[i].y, ast[i].c);
                    floodfill(ast[i].x+(ast[i].s)-1, ast[i].y, ast[i].c);
                    floodfill(ast[i].x, ast[i].y-(ast[i].s)+1, ast[i].c);
                    floodfill(ast[i].x, ast[i].y+(ast[i].s)-1, ast[i].c);
               end;
               if (ast[i].h<=50) then
               begin
                    if ast[i].h mod 2=1 then
                    begin
                         circle(ast[i].x+4, ast[i].y-5+random(2),5);
                         circle(ast[i].x-6+random(2), ast[i].y+5,5);
                         floodfill(ast[i].x-4, ast[i].y+5,ast[i].c);
                         floodfill(ast[i].x+6, ast[i].y-5,ast[i].c);
                    end
                    else begin
                         circle(ast[i].x+4, ast[i].y+5,7);
                         floodfill(ast[i].x+4, ast[i].y+5,ast[i].c);
                         putpixel(ast[i].x+8, ast[i].y-6+random(2),ast[i].c);
                         putpixel(ast[i].x-7+random(3), ast[i].y+5,ast[i].c);
                    end;
               end;
          end;
     end;
end;

procedure drawenmys;
var i,s:integer;
begin
     if nmy[enmys].y < 15 then
             for s:= 1 to enmys do nmy[s].y := nmy[s].y+1;
     for i:=1 to enmys do
     begin
          if nmy[i].h > 0 then
          begin
               nmy[i].l:=nmy[i].l+1;
               if nmy[i].l=58 then nmy[i].l:=1;
               setcolor(245);
               setfillstyle(1,225);
               arc(nmy[i].x+23+move[nmy[i].l] ,nmy[i].y-25 ,227,313,34);
               arc(nmy[i].x-18+move[nmy[i].l] ,nmy[i].y+38 ,4,65,41);
               arc(nmy[i].x+63+move[nmy[i].l] ,nmy[i].y+38 ,115,176,41);
               floodfill(nmy[i].x+23+move[nmy[i].l] ,nmy[i].y+25 ,245);
               arc(nmy[i].x+move[nmy[i].l], nmy[i].y+35, 10,38,40);
               arc(nmy[i].x+move[nmy[i].l]+45, nmy[i].y+35, 142, 170, 40);
               arc(nmy[i].x+move[nmy[i].l], nmy[i].y+35, 10,38,39);
               arc(nmy[i].x+move[nmy[i].l]+45, nmy[i].y+35, 142, 170, 39);
               arc(nmy[i].x+move[nmy[i].l]+23, nmy[i].y+35, 70, 110, 29);
               setfillstyle(1,220);
               floodfill(nmy[i].x+move[nmy[i].l]+23,nmy[i].y+8, 245);
          end;
     end;
end;

procedure drawshots;
var i :integer;
begin
     for i:=1 to limit do
         if (shot[i].p = 1) and (shot[i].s=true)then
         begin
              setcolor(lightblue);
              line(shot[i].x, shot[i].y, shot[i].x, shot[i].y -10);
              setcolor(blue);
              line(shot[i].x -1, shot[i].y +1, shot[i].x -1, shot[i].y -9);
              line(shot[i].x +1, shot[i].y +1, shot[i].x +1, shot[i].y -9);
         end
         else if (shot[i].p = 2) and (shot[i].s=true) then
         begin
              setcolor(40);
              line(shot[i].x, shot[i].y, shot[i].x, shot[i].y -10);
              setcolor(112);
              line(shot[i].x -1, shot[i].y +1, shot[i].x -1, shot[i].y -9);
              line(shot[i].x +1, shot[i].y +1, shot[i].x +1, shot[i].y -9);
              setcolor(184);
              line(shot[i].x -2, shot[i].y +2, shot[i].x -2, shot[i].y -8);
              line(shot[i].x +2, shot[i].y +2, shot[i].x +2, shot[i].y -8);
         end
         else if (shot[i].p = 3) and (shot[i].s = true) then
         begin
              setcolor(46);
              circle(shot[i].x, shot[i].y, 2);
              setcolor(118);
              circle(shot[i].x, shot[i].y, 3);
              setcolor(190);
              circle(shot[i].x, shot[i].y, 4);
         end;
end;

procedure firehit;
var i,i2:integer; a,b,c:real;
begin

     for i:=1 to limit do
         if shot[i].s = true then
         begin
              dec(shot[i].y,14);
              for i2:=1 to astrod do
              begin
                   a:=abs(shot[i].y - ast[i2].y);
                   b:=abs(shot[i].x - ast[i2].x);
                   c:=sqrt(sqr(a) +sqr(b));
                   if c < ast[i2].s+5 then
                   begin
                        hit(shot[i].x,shot[i].y,20);
                        shot[i].s := false;          {make slot open}
                        fire[1] := fire[1]-1;    {subtract from total shots}
                        if shot[i].p=1 then dec(ast[i2].h,59-random(2));
                        if shot[i].p=2 then dec(ast[i2].h,100);
                        if shot[i].p=3 then dec(ast[i2].h,150);
                        if ast[i2].h <= 0 then
                        begin
                             stats[6]:=stats[6]+100;     {add 100 points}
                             ast[i2].y:=500;             {find new asteroid coordanates}
                             hit(shot[i].x,shot[i].y,30);{show explosion}
                        end;
                   end;
              end;
         end;
     {
     setcolor(red);
     for i:=1 to enmys do
         circle(nmy[i].x+move[nmy[i].l]+23, nmy[i].y+18, 17);
         {rectangle(nmy[i].x+move[nmy[i].l],nmy[i].y,nmy[i].x+move[nmy[i].l]+45,nmy[i].y+35);}

     if (clock>=850) then
        for i:=1 to limit do
        begin
             dec(shot[i].y,14);
             for i2:=1 to enmys do
                 if (shot[i].s =true) and (nmy[i2].h >0) then
                 begin
                      a:=abs((shot[i].y) - (nmy[i2].y) - 18);
                      b:=abs((shot[i].x) - (nmy[i2].x + move[nmy[i2].l]) - 23);
                      c:=sqrt(sqr(a) +sqr(b));
                      if c <= 17 then
                      begin
                           hit(shot[i].x,shot[i].y,20);
                           shot[i].s := false;      {make slot open}
                           fire[1] := fire[1]-1;    {subtract from total shots}
                           if shot[i].p=1 then dec(nmy[i2].h,50);
                           if shot[i].p=2 then dec(nmy[i2].h,100);
                           if shot[i].p=3 then dec(nmy[i2].h,150);
                           if nmy[i2].h <= 0 then
                           begin
                                inc(stats[6],100);     {add 100 points}
                                hit(shot[i].x, shot[i].y, 30);{show explosion}
                           end;
                      end;
                 end;
        end;
     for i:=1 to limit do
         if shot[i].y < 0 then  {if it missed}
         begin
              shot[i].s := false;
              fire[1] := fire[1]-1;
         end;
end;

procedure movestars;
var i :integer;
begin
     for i:=1 to density do
     begin
          stars[i].y:=stars[i].y+1;
          if stars[i].y>480 then
          begin
             stars[i].y:=0-random(150);
             stars[i].x:=random(640);
             if random(2)=0 then stars[i].c:=random(4)+28
             else stars[i].c:=random(5)+80;
          end;
     end;
     for i:=1 to density do
         putpixel(stars[i].x, stars[i].y, stars[i].c);
end;

procedure blowupship;
var i,th,s,x,y :integer;
    c      :array[1..4] of integer;
begin
     for i:=1 to 50 do
     begin
          setcolor(lightblue);
          th:=(i div 3) * 2;
          setfillstyle(1,white);
          fillellipse(pos.x, pos.y, i - (i div 9),i - (i div 9));
          fillellipse(pos.x,pos.y,i,th);
          fillellipse(pos.x,pos.y,th,i);
          sound(100+(1 div 5));
     end;
     for i:=1 to 8 do
     begin
          x:=pos.x+random(200)-100;
          y:=pos.y+random(200)-100;
          for s:=1 to 15 do
          begin
               fillellipse(x,y,s,s);
          end;
     end;
     nosound;
     clearviewport;
     floodfill(1,1,red);
     setcolor(blue);
     settextstyle(6,0,7);
     outtextxy(200,200,'You Lost');
     outtextxy(201,200,'You Lost');
     outtextxy(200,201,'You Lost');
     readkey;
end;

procedure checkhit;
var i,i2 :integer; a,b,c:real;
begin
     for i:=1 to astrod do
         if (ast[i].h >0) then
         begin
              a:=abs((ast[i].y) -(pos.y)+8);
              b:=abs((ast[i].x) -(pos.x));
              c:=sqrt(sqr(a) +sqr(b));
              if c <= 25 then
              begin
                   hit(ast[i].x, ast[i].y,30);
                   ast[i].y:=600;
                   if ast[i].h <=50 then dec(stats[1],5)
                   else dec(stats[1],10);
              end;
         end;
end;

procedure pause;
var leave   :boolean;
    focus,x,check :integer;
begin
     focus:=1;
     leave:=false;
     setcolor(white);
     outtextxy(280,160,'Paused');
     line(270,185,270,280);
     line(280,175,380,175);
     line(270,185,280,175);
     settextstyle(6,0,3);
     repeat
           settextstyle(6,0,3);
           setcolor(224);
           if focus<>1 then outtextxy(280,175,'Continue');
           if focus<>2 then outtextxy(280,200,'Save Game >');
           if focus<>3 then outtextxy(280,225,'Load Game >');
           if focus<>4 then outtextxy(280,250,'Quit');
           setcolor(54);
           if focus = 1 then outtextxy(280,175,'Continue');
           if focus = 2 then outtextxy(280,200,'Save Game >');
           if focus = 3 then outtextxy(280,225,'Load Game >');
           if focus = 4 then outtextxy(280,250,'Quit');
           setcolor(black);
           outtextxy(290,300,'Loaded');
           outtextxy(290,300,'Saved');
           if (keypressed) and (check mod 110 =0) then
           begin
                if up   =true then focus:=focus-1;
                if down =true then focus:=focus+1;
                if ((focus=2) or (focus=3)) and (x=0) and (right =true)then x:=x+1;
                if ((focus=2) or (focus=3)) and (x=1) and (left  =true)then x:=x-1;
                if enter = true then
                case focus of
                     1: begin leave:=true; esc:=false;end;
                     2: save;
                     3: load;
                     4: begin leave:=true;exit:=true;end;
                end;
                check:=0;
                wait(10);
           end;
           if focus = 0 then focus:=4;
           if focus = 5 then focus:=1;
           inc(check,1);
     until leave =true;
end;

procedure display(st:string; stage:integer);
var x,width,z :integer;
    i,y:integer;
begin
     {settextstyle(2,0,4);
     setcolor(red);
     width:=textwidth(what[u]);
     if width>85 then z:=300-(width div 2) else begin z:=250; width:=85;end;
     outtextxy(z-(width div 2),468,what[u]);
     setcolor(lightblue);
     outtextxy(z-(textwidth('Message') div 2),457,'Message');
     setcolor(white);
     rectangle(290-width,469,300,480);
     ellipse(310-width,469,90,180,20,10);
     line(320,458,310-width,458);
     if show > 0 then show:=show-1;}
     if stage+16 < 31 then setcolor(stage+16)
     else if ((stage+16)>=31) and (stage+16<84)then setcolor(white)
     else if (stage+16>=84) then setcolor(abs(84-stage));
     settextstyle(3,0,3);
     x:=round(320- (textwidth(st)/2) );
     outtextxy(x,400,st);
     if show < 100 then inc(show,1) else show:=0;
end;

procedure initvar;
begin
     for i:=1 to density do
     begin
         stars[i].x:=random(640);
         stars[i].y:=random(960)-480;
         if random(2)=0 then stars[i].c:=random(4)+28
         else stars[i].c:=random(5)+80;
     end;
     for i:=1 to limit do shot[i].p:=0;
     for i:=1 to astrod do
     begin
          ast[i].x:=random(560)+40;
          ast[i].y:=random(480)-480;
          ast[i].s:=9+random(3);
          ast[i].h:=100;
          ast[i].io:=true;
          ast[i].c:=random(5)+20;
     end;
     for i:=1 to limit do shot[i].s := false;
     s:=0;
     for i:= 1 to enmys do
     begin
          nmy[i].x := 20+(50*(i-(10*s)));
          nmy[i].y :=-40+ (-40 * s);
          nmy[i].h := 150;
          nmy[i].l := 25-(s*4);
          if i mod 10 = 0 then s:=s+1;
     end;
     what[2]:='Weapon Upgrade'; what[3]:='Low Power!';
     what[4]:='Weapon Energy Low!!';
     fire[1]:=0; fire[5]:=0;clock:=00;
     pos.x:=320; pos.y:=400; show:=0;
     exit:=false;
     stats[1]:=100; stats[2]:=1; stats[3]:=0;
     stats[4]:=1;   stats[5]:=1; stats[6]:=0;
     stats[7]:=100; {power:available weapons:points:weapon used:level:wepean power}
end;

procedure game;
begin
     initvar;
     xkey;
     repeat
     {Drawing section}
           movestars;
           firehit;
           drawshots;
           drawastro;
           for i:=1 to astrod do
               if ast[i].io=false then inc(j,1);
           if (j=astrod) and (clock>=600) then drawenmys;
           drawship;
           checkhit;
           statistics;
           {Keyboard section}
           if esc  =true then pause;
           if up   =true then dec(pos.y,4);
           if down =true then inc(pos.y,4);
           if left =true then dec(pos.x,5);
           if right=true then inc(pos.x,5);
           if (F1  =true) then stats[4]:=1;
           if (F2  =true) and (stats[2]>=2) then stats[4]:=2;
           if (F3  =true) and (stats[2]>=3) then stats[4]:=3;
           if (F4  =true) and (stats[2]>=4) then stats[4]:=4;
           if space=true then
           begin
                if (fire[1] < limit) and (fire[5] <= 0) and (stats[7]>3)then
                begin
                      for i:= 1 to limit do         {is a slot open}
                          if shot[i].s = false then {if so what slot is open}
                             fire[2] := i;          {assign fire[2] with open slot}
                      if stats[4]=1 then
                         dec(stats[7],2);           {take away some power}
                      if stats[4]=2 then
                         dec(stats[7],3);
                      if stats[4]=3 then
                         dec(stats[7],8);
                      shot[fire[2]].x:=pos.x;       {get shots position}
                      shot[fire[2]].y:=pos.y-15;    {get shots position}
                      shot[fire[2]].p:=stats[4];    {what weapon was used}
                      shot[fire[2]].s:=true;        {make that slot closed}
                      fire[1]:=fire[1]+1;           {add 1 to total amout of shots}
                      if stats[4]=1 then fire[5]:=3;
                      if stats[4]=2 then fire[5]:=5;
                      if stats[4]=3 then fire[5]:=10;
                end;
           end;
     {Varible check and change section}
           if pos.x > 610 then pos.x:= 610; {set boundries}
           if pos.x < 30  then pos.x:= 30;
           if pos.y > 450 then pos.y:= 450;
           if pos.y < 300 then pos.y:= 300;

           if (stats[1] <= 40) and (show<=0)then
           begin
                display('Shields low',show);
           end;
           if stats[7] <= 45 then
           begin
                display('Low Energy',show);
           end;
           if (stats[3]=2000) then
           begin
                u:=2;
                show:=30;
                stats[4]:=2;stats[2]:=2;
           end;
           if (stats[3]=6000) then
           begin
                u:=2;
                show:=30;
                stats[4]:=3;stats[2]:=3;
           end;
           if fire[5]>0 then dec(fire[5],1);
           if (stats[1] < 100) and (clock mod 10 =0) then inc(stats[1],1);
           if (stats[7] < 100) and (clock mod 8 = 0) then inc(stats[7],2);
           inc(clock,1);
           j:=0;
           if stats[6] > 0 then
           begin
                inc(stats[3],5);
                dec(stats[6],5);
           end;
           wait(20000);
           setfillstyle(1,black);
           bar(0,0,640,480);
     until (exit = true);
     if stats[1] <= 0 then blowupship;
     closegraph;
     nkey;
     textmode(co80);
end;

var gd,gm:integer;
begin
     randomize;
     {clrscr;
     init;
     Wait(1000);
     mission;}
     chdir('C:\tp\bgi');
     gd := detect; initgraph(gd,gm,'');
     {logo;}
     game;
     nkey;
end.