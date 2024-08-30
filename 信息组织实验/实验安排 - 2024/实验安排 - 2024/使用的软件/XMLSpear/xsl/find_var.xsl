<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions">
	<xsl:output encoding="UTF-8" method="text" />

	<xsl:template match="*">
		<xsl:copy>
			<xsl:apply-templates select="@*" />
			<xsl:apply-templates />
		</xsl:copy>
	</xsl:template>

    <!-- <xsl:template match="attribute()" > not supported in xalan -->
	<xsl:template match="@*">
		<xsl:call-template name="replace-escapes">
			<xsl:with-param name="string" select="." />
		</xsl:call-template>
	</xsl:template>


	<xsl:template match="text()">
		<xsl:call-template name="replace-escapes">
			<xsl:with-param name="string" select="." />
		</xsl:call-template>
	</xsl:template>

	<xsl:template name="replace-escapes">
		<xsl:param name="string" />
		<xsl:choose>
			<xsl:when test="contains($string, '[$')">
				<!-- replacement for the string between the []s -->
				<xsl:variable name="ename"
					select="substring-before(substring-after($string, '[$'), ']')" />
				<xsl:value-of select="$ename" />
				<xsl:text>&#xa;</xsl:text>
				<!-- recursive call on the rest of the string -->
				<xsl:call-template name="replace-escapes">
					<xsl:with-param name="string"
						select="substring-after(substring-after($string, '[$'), ']')" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>

			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
