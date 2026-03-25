import codecs
p1 = r'e:\semester 4\Projects\wify\DigitalDivide\app 2.R'
p2 = r'e:\semester 4\Projects\wify\DigitalDivide\app3.R'
p3 = r'e:\semester 4\Projects\wify\DigitalDivide\style.css'

with codecs.open(p1, 'r', 'utf-8') as f:
    content = f.read()

start_marker = 'tags(HTML(\"'
end_marker = '\"))'

idx1 = content.find(start_marker)
if idx1 != -1:
    idx2 = content.find(end_marker, idx1)
    css_content = content[idx1 + len(start_marker):idx2]
    
    with codecs.open(p3, 'w', 'utf-8') as f:
        f.write(css_content.strip() + '\n')
        
    new_content = content[:idx1] + 'includeCSS(\"style.css\")' + content[idx2 + len(end_marker):]
    with codecs.open(p2, 'w', 'utf-8') as f:
        f.write(new_content)
    print('Done!')
else:
    print('Not found')
