<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:data="urn:var.table"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions">
	<xsl:output encoding="UTF-8" method="xml" />

	<!-- in xalan declare xmlns:exslt="http://exslt.org/common and use  -->
	<!-- select="exslt:node-set($replacements)//entry[@key=$ename])" />-->
	<xsl:param name="replacements">
		<replacements>
			<entry key="userName">pw78ds</entry>
			<entry key="externalId">PW78DS</entry>
			<entry key="lastName">Deneer</entry>
			<entry key="firstName">Dick</entry>
			<entry key="resourceId">pw78ds</entry>
			<entry key="emailAddress">d.deneer@chello.nl</entry>
		</replacements>
	</xsl:param>

	<xsl:template match="*">
		<xsl:copy>
			<xsl:apply-templates select="@*" />
			<xsl:apply-templates />
		</xsl:copy>
	</xsl:template>

	<!-- necessary to copy all namespace declarations of root -->
	<xsl:template match="/*">
		<xsl:copy>
		    <!--   not supported in xalan  -->
		    <!-- <xsl:copy-of select="@*, //namespace::*" /> -->
			<xsl:copy-of select="@*"/>
			<xsl:apply-templates />
		</xsl:copy>
	</xsl:template>

     <!-- <xsl:template match="attribute()" > not supported in xalan -->
	<xsl:template match="@*">
		<xsl:variable name="attrName" select="name()"></xsl:variable>
		<xsl:attribute name="{$attrName}">
		<xsl:call-template name="replace-escapes">
			<xsl:with-param name="string" select="." />
		</xsl:call-template>
		</xsl:attribute>

	</xsl:template>



	<!-- you can also use this specific xpath to find text that must be substituted-->
	<!--	<xsl:template match="text()[$contains(.,'[$') and contains(.,']')]">
		<xsl:call-template name="replace-escapes">
		<xsl:with-param name="string" select="."/>
		</xsl:call-template>
		</xsl:template>-->
	<xsl:template match="text()">
		<xsl:call-template name="replace-escapes">
			<xsl:with-param name="string" select="." />
		</xsl:call-template>
	</xsl:template>
	<xsl:template name="replace-escapes">
		<xsl:param name="string" />
		<xsl:choose>
			<xsl:when test="contains($string, '[$')">
				<!-- substring before the escaped sequence -->
				<xsl:value-of select="substring-before($string, '[$')" />
				<!-- replacement for the string between the [$]s -->
				<xsl:variable name="ename"
					select="substring-before(substring-after($string, '[$'), ']')" />
				<xsl:value-of
					select="$replacements//entry[@key=$ename]" />
				<!-- recursive call on the rest of the string -->
				<xsl:call-template name="replace-escapes">
					<xsl:with-param name="string"
						select="substring-after(substring-after($string, '[$'), ']')" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$string" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
