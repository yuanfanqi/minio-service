package com.minioservice.entity;

import java.util.Date;
import java.io.Serializable;

/**
 * minio文件操作记录(LogMinioOperate)实体类
 *
 * @author makejava
 * @since 2023-02-08 14:31:25
 */
public class LogMinioOperate implements Serializable {
    private static final long serialVersionUID = 564724005211105784L;
    /**
     * 主键
     */
    private Long id;
    /**
     * 文件名
     */
    private String fileName;
    /**
     * 来源服务ID
     */
    private String sourceService;
    /**
     * 文件存储路径
     */
    private String addrMinio;
    /**
     * 文件hash编码
     */
    private String fileHash;
    /**
     * 文件描述
     */
    private String fileDesc;
    /**
     * 操作(0-查询，1-上传，2-下载，3-删除)
     */
    private String operate;
    /**
     * 覆盖时间
     */
    private Date replaceTime;
    /**
     * 是否删除(0-false;1-true)
     */
    private String isDelete;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 创建人
     */
    private String createUser;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 修改人
     */
    private String modifyUser;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getSourceService() {
        return sourceService;
    }

    public void setSourceService(String sourceService) {
        this.sourceService = sourceService;
    }

    public String getAddrMinio() {
        return addrMinio;
    }

    public void setAddrMinio(String addrMinio) {
        this.addrMinio = addrMinio;
    }

    public String getFileHash() {
        return fileHash;
    }

    public void setFileHash(String fileHash) {
        this.fileHash = fileHash;
    }

    public String getFileDesc() {
        return fileDesc;
    }

    public void setFileDesc(String fileDesc) {
        this.fileDesc = fileDesc;
    }

    public String getOperate() {
        return operate;
    }

    public void setOperate(String operate) {
        this.operate = operate;
    }

    public Date getReplaceTime() {
        return replaceTime;
    }

    public void setReplaceTime(Date replaceTime) {
        this.replaceTime = replaceTime;
    }

    public String getIsDelete() {
        return isDelete;
    }

    public void setIsDelete(String isDelete) {
        this.isDelete = isDelete;
    }

    public Date getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }

    public String getCreateUser() {
        return createUser;
    }

    public void setCreateUser(String createUser) {
        this.createUser = createUser;
    }

    public Date getModifyTime() {
        return modifyTime;
    }

    public void setModifyTime(Date modifyTime) {
        this.modifyTime = modifyTime;
    }

    public String getModifyUser() {
        return modifyUser;
    }

    public void setModifyUser(String modifyUser) {
        this.modifyUser = modifyUser;
    }

}

