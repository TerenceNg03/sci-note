import {Tag, Space} from 'antd';
import {ConfigProvider} from 'antd';
import enUSIntl from 'antd/lib/locale/en_US';
import {ProDescriptions} from '@ant-design/pro-components';
import {React, useRef} from 'react';
import {Editable, useEditor} from "@wysimark/react"
import {UploadOutlined} from '@ant-design/icons'

const paper = {
    name: 'Software transactional memory',
    author: 'N Shavit, D Touitou',
    uuid: '123456',
    cite: 'Shavit, Nir, and Dan Touitou. "Software transactional memory." Proceedings of the fourteenth annual ACM symposium on Principles of distributed computing. 1995.',
    url: 'https://dl.acm.org/doi/pdf/10.1145/1400214.1400228',
    file: '/Users/<user-name>/sci-note/db.json',
    tags: ['abc', 'def'],
    notes:
        `# Title 
\`\`\`javascript
function greet(name) {
console.log('Hello, ' + name + '!');
}

greet('John');
\`\`\`

### HTML`
}

const columns = [
    {
        title: 'title',
        key: 'title',
        dataIndex: 'title',
        valueType: 'text',
    },
    {
        title: 'author',
        key: 'author',
        dataIndex: 'author',
        valueType: 'text',
    },
    {
        title: 'url',
        key: 'url',
        dataIndex: 'url',
        valueType: 'text',
        render: (dom, entity, index, action) => {
            return (
                <a href={
                    "javascript:window.require('electron').shell.openExternal(" + JSON.stringify(dom) + ")"
                }>{dom}</a>
            );
        },
    },
    {
        title: 'file',
        key: 'file',
        dataIndex: 'file',
        valueType: 'text',
        editable: false,
        render: (dom, entity, index, action) => {
            return (
                <Space>
                    <a href={
                        "javascript:window.require('electron').shell.openPath(" + JSON.stringify(dom) + ")"
                    }>{dom}</a>
                    <UploadOutlined onClick={() => {
                        const {ipcRenderer} = window.require('electron');
                        ipcRenderer.send('open-file',
                            {
                                defaultPath: JSON.stringify(dom),
                                properties: ['openFile'],
                            }
                        );
                        ipcRenderer.on('select-paper', (event, response) => {
                            if (!response.canceled) {
                                console.log(response.filePaths[0]);
                            }
                        })
                    }} />
                </Space>
            );
        },
    },
    {
        title: 'cite',
        key: 'cite',
        dataIndex: 'cite',
        valueType: 'text',
    },
]

const Tags = (props) => {
    let l = props.tags.map(
        (x) => {return <Tag style={{fontSize: '120%', padding: '0.3em 0.5em'}}>{'#' + x}</Tag>}
    )
    return <Space size={'middle'} style={{marginBottom: '1em'}}>{l}</Space>
}

const Desc = (props) => {
    const actionRef = useRef();
    return (
        <ProDescriptions
            actionRef={actionRef}
            bordered
            style={{paddingBottom: '1em'}}
            formProps={{
                onValuesChange: (e, f) => console.log(e, f),
            }}
            title=""
            request={async () => {return Promise.resolve({success: true, data: props.paper})}}
            editable={{
            }}
            column={1}
            columns={columns}
        />
    )
}

const Paper = (props) => {
    const editor = useEditor({})
    let paper = props.paper;
    if (paper.paper.length === 0){
        return <div />
    }
    let tags = paper.tags;
    paper = paper.paper[0];
    console.log(paper)
    return (
        <ConfigProvider locale={enUSIntl}>
            <div style={{fontSize: '120%', padding: '0em 1em 1em', overflowY: 'scroll', height: '100%'}}>
                <h1>{paper.title}</h1>
                <Tags tags={tags} />
                <Desc paper={paper} />
                <Editable
                    editor={editor}
                    value={paper.notes}
                    onChange={
                        (x) => {console.log('Markown change', x)}
                    }
                />
            </div>
        </ConfigProvider>
    )
}

export default Paper;
