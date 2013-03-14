#!/usr/bin/env luajit

local my_assert=assert
local push = function(arr,item)
   arr[#arr+1]=item
end

local function get_file_as_t (file)
   local content = {}
   local i = 0;
   for line in io.lines(file) do
      push(content,line)
   end
   return content
end

local get_items = function (content)
   local items = {}
   local find=string.find
   local match=string.match
   local format=string.format
   local key,item=nil,{}
   for i,line in ipairs(content) do
      if line:find([[^"]]) then
	 my_assert(key,format("tailing string in wrong position,line number: %d\nline:%s",i,line))
	 if key ~= "m-other" then
	    item[key]=item[key] .. line:match([["(.*)"]])
	 end
      elseif line:find([[^msgid%s+"]]) or line:find([[^msgid%[0%]%s+]]) then
	 key="msgid"
	 my_assert(not item[key],format("duplicate %s,line number:%d",key,i))
	 item[key]=line:match([["(.*)"]])
      elseif line:find([[^msgstr%s+"]]) or line:find([[^msgstr%[0%]%s+]]) then
	 key = "msgstr"
	 my_assert(not item[key],format("duplicate %s,line number:%d",key,i))
	 item[key]=line:match([["(.*)"]])
      elseif line:find([[^msgctxt%s+]]) then
	 key = "msgctxt"
	 my_assert(not item[key],format("duplicate %s,line number:%d",key,i))
	 item[key]=line:match([["(.*)"]])
      elseif line:find([[^#,]]) then
	 key = "flag"
	 my_assert(not item[key],format("duplicate %s,line number:%d",key,i))
	 item[key]=line:match([[^#,%s*(.*)]]):gsub("%s+","")
      elseif line:find([[^%s*$]]) then
	 if item["msgid"] and item["msgstr"] then
	    push(items,item)
	    item={}
	 end
      elseif line:find([[^#~]]) then
	 item["flag"]=nil
      elseif line:find [[^#]] then
      elseif line:find [[^m]] then
	 key = "m-other"
	 item[key]=line:match([["(.*)"]])
      else
	 my_assert(nil,format("undefined format for {%s},line number:%d",line,i))
      end
   end
   if item["msgid"] and item["msgstr"] then
      push(items,item)
      item={}
   end
   return items
end


local function headerinfo (content)
   local trans,trans_e,team,team_e,charset,pf
   local match = string.match
   local find = string.find
   for _,line in ipairs(content) do
      -- "Last-Translator: Yinghua Wang <wantinghard@gmail.com>\n"
      if(find(line,[[^"Last%-Translator]])) then
      	 trans,trans_e=match(line,[[^"Last%-Translator: *([^<]+[^ <]) *<([^>]+)>]])
      	 -- "Language-Team: Chinese (simplified) <i18n-zh@googlegroups.com>\n"
      elseif(find(line,[[^"Language%-Team:]])) then
      	 team,team_e=match(line,[[^"Language%-Team: *([^<]+[^ <]) *<([^>]+)>]])
      elseif(find(line,[[^"Content%-Type]])) then
      	 charset=match(line,[[^"Content%-Type: text/plain; charset=([^ ]+) *\n]])
      	 -- "Plural-Forms: nplurals=1; plural=0;\n"
      elseif(find(line,[[^"Plural%-Forms: ]])) then
      	 pf=match(line,[[^"Plural%-Forms: *(.+[^ ]) *\n"]])
      end
   end
   return trans,trans_e,team,team_e,charset,pf
end

local function main ()
   assert(#arg>=1,"need one argument\n")
   local format=string.format
   local luasql=require("luasql.sqlite3")
   local clock=os.clock
   local tm1,tm2=0,0
   local env = assert (luasql.sqlite3())
   local con = assert (env:connect("test.sqlite"))
   local e = function (t,i)
      if t[i] then
	 return con:escape(t[i])
      else
	 return ""
      end
   end
   local res = assert (con:execute[[BEGIN TRANSACTION]])
   res = assert (con:execute[[create table 'tab' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)]])
   for _,file in ipairs(arg) do
      tm1=tm1-clock()
      local content=get_file_as_t(file)
      local items=get_items(content)
      tm1=tm1+clock()
      file=con:escape(file)
      tm2=tm2-clock()
      for i,item in ipairs(items) do
	 res = assert(con:execute(format("insert into 'tab' values(%d,'%s','%s','%s',1,'aou','%s')",
					 i,e(item,'msgid'),e(item,'msgstr'),e(item,'msgctxt'),file)),format("%s,line number:%d",item["msgstr"],i))
      end
      tm2=tm2+clock()
   end
   tm2=tm2-clock()
   res = assert(con:execute("create index 'idx' on 'tab' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)"))
   res = assert (con:commit())
   tm2=tm2+clock()
   print(tm1,tm2)
end

main()
